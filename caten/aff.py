# caten/aff.py
"""
Pure Python implementation of ISL-like affine relation algebra.

This module implements symbolic affine access relation analysis without
depending on the ISL (Integer Set Library). It supports:
- Symbolic coefficients (ATenOp expressions) for parametric shapes
- Relation composition for dependency analysis
- Existential variable elimination for fusion feasibility

Mathematical Foundation:
========================
An access relation R ⊆ I × M maps iteration vectors to memory addresses.
For a statement accessing A[f(i,j,k)] where f is affine:
    R = { S[i,j,k] -> [f(i,j,k)] }

Dependency analysis computes:
    d = R^{-1} ∘ W = { (i,i') | ∃m: W(i)=m ∧ R(i')=m }
          = { (i,i') | W(i) = R(i') }

This tells us which iteration pairs access the same memory location,
which is essential for determining if loops can be fused.

Key Insight for Multi-Strided Arrays:
=====================================
For an array A[d0, d1, d2] with strides [s0, s1, s2], the linearized address is:
    addr = s0*i0 + s1*i1 + s2*i2 + offset

We represent this as a single affine expression over iteration variables,
NOT using lexicographic order. This simplifies the algebra significantly:
- Composition just requires matching the intermediate (address) space
- Variables are eliminated via substitution when coefficient is ±1

References:
===========
1. ISL Manual: "isl is a thread-safe C library for manipulating sets and
   relations of integer points bounded by affine constraints."
2. Verdoolaege, "isl: An Integer Set Library for the Polyhedral Model" (2010)
3. ISL GitHub: https://github.com/Meinersbur/isl
"""
from __future__ import annotations

from dataclasses import dataclass, field
from typing import (
    TYPE_CHECKING,
    Any,
    Dict,
    FrozenSet,
    List,
    Mapping,
    Optional,
    Sequence,
    Set,
    Tuple,
    Union,
)

if TYPE_CHECKING:
    from caten.ir import ATenOp

# Type alias for coefficients: can be integer or symbolic (ATenOp)
Coeff = Union[int, "ATenOp"]


def _is_aten_op(x: Any) -> bool:
    """Check if x is an ATenOp without importing ir at module level."""
    return hasattr(x, "T") and hasattr(x, "args")


def _coeff_is_zero(c: Coeff) -> bool:
    """Check if coefficient is zero (handles both int and ATenOp)."""
    if isinstance(c, int):
        return c == 0
    # For ATenOp, check if it's a Const with value 0
    if hasattr(c, "value") and hasattr(c, "__class__"):
        if c.__class__.__name__ == "Const":
            return c.value == 0
    return False


def _coeff_is_one(c: Coeff) -> bool:
    """Check if coefficient is 1."""
    if isinstance(c, int):
        return c == 1
    if hasattr(c, "value") and c.__class__.__name__ == "Const":
        return c.value == 1
    return False


def _coeff_is_neg_one(c: Coeff) -> bool:
    """Check if coefficient is -1."""
    if isinstance(c, int):
        return c == -1
    if hasattr(c, "value") and c.__class__.__name__ == "Const":
        return c.value == -1
    return False


def _coeff_add(a: Coeff, b: Coeff) -> Coeff:
    """Add two coefficients."""
    if isinstance(a, int) and isinstance(b, int):
        return a + b
    # Convert to ATenOp if needed
    from caten.ir import Add, Const, index
    if isinstance(a, int):
        a = Const.new(a, index)
    if isinstance(b, int):
        b = Const.new(b, index)
    return Add((a, b))


def _coeff_mul(a: Coeff, b: Coeff) -> Coeff:
    """Multiply two coefficients."""
    if isinstance(a, int) and isinstance(b, int):
        return a * b
    from caten.ir import Mul, Const, index
    if isinstance(a, int):
        a = Const.new(a, index)
    if isinstance(b, int):
        b = Const.new(b, index)
    return Mul((a, b))


def _coeff_neg(a: Coeff) -> Coeff:
    """Negate a coefficient."""
    if isinstance(a, int):
        return -a
    from caten.ir import Neg
    return Neg((a,))


def _coeff_eq(a: Coeff, b: Coeff) -> bool:
    """Check coefficient equality (conservative for symbolic)."""
    if isinstance(a, int) and isinstance(b, int):
        return a == b
    # For ATenOp, use object identity or structural equality
    return a == b


# =============================================================================
# AffExpr: Affine Expression with Symbolic Coefficients
# =============================================================================
@dataclass(frozen=False, eq=False)
class AffExpr:
    """
    Affine expression: const + Σ coeff[var] * var

    Variables are strings like "gid0", "gid1", "addr", etc.
    Coefficients can be integers or ATenOp expressions (symbolic).

    Example:
        1500*gid0 + 30*gid1 + gid2  ->  AffExpr({"gid0": 1500, "gid1": 30, "gid2": 1}, 0)
    """
    coeff: Dict[str, Coeff] = field(default_factory=dict)
    const: Coeff = 0

    def __post_init__(self) -> None:
        # Remove zero coefficients
        if self.coeff:
            self.coeff = {v: c for v, c in self.coeff.items() if not _coeff_is_zero(c)}

    def __hash__(self) -> int:
        # Hash based on sorted coefficient items and constant
        coeff_items = tuple(sorted((k, id(v) if not isinstance(v, int) else v) for k, v in self.coeff.items()))
        const_hash = id(self.const) if not isinstance(self.const, int) else self.const
        return hash((coeff_items, const_hash))

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, AffExpr):
            return False
        if set(self.coeff.keys()) != set(other.coeff.keys()):
            return False
        for k in self.coeff:
            if not _coeff_eq(self.coeff[k], other.coeff.get(k, 0)):
                return False
        return _coeff_eq(self.const, other.const)

    @staticmethod
    def var(name: str) -> "AffExpr":
        """Create single variable expression."""
        return AffExpr({name: 1}, 0)

    @staticmethod
    def const_(c: Coeff) -> "AffExpr":
        """Create constant expression."""
        return AffExpr({}, c)

    @staticmethod
    def zero() -> "AffExpr":
        """Create zero expression."""
        return AffExpr({}, 0)

    def variables(self) -> FrozenSet[str]:
        """Return set of variables with non-zero coefficients."""
        return frozenset(v for v, c in self.coeff.items() if not _coeff_is_zero(c))

    def coeff_of(self, var: str) -> Coeff:
        """Get coefficient of a variable (0 if not present)."""
        return self.coeff.get(var, 0)

    def is_zero(self) -> bool:
        """Check if expression is identically zero."""
        return not self.coeff and _coeff_is_zero(self.const)

    def __add__(self, other: "AffExpr | int") -> "AffExpr":
        if isinstance(other, int):
            return AffExpr(dict(self.coeff), _coeff_add(self.const, other))
        # Merge coefficients
        new_coeff: Dict[str, Coeff] = dict(self.coeff)
        for v, c in other.coeff.items():
            new_coeff[v] = _coeff_add(new_coeff.get(v, 0), c)
        return AffExpr(new_coeff, _coeff_add(self.const, other.const))

    def __radd__(self, other: int) -> "AffExpr":
        return self + other

    def __neg__(self) -> "AffExpr":
        return AffExpr(
            {v: _coeff_neg(c) for v, c in self.coeff.items()},
            _coeff_neg(self.const)
        )

    def __sub__(self, other: "AffExpr | int") -> "AffExpr":
        if isinstance(other, int):
            return self + (-other)
        return self + (-other)

    def __mul__(self, k: Coeff) -> "AffExpr":
        """Scalar multiplication."""
        if _coeff_is_zero(k):
            return AffExpr.zero()
        if _coeff_is_one(k):
            return self
        return AffExpr(
            {v: _coeff_mul(c, k) for v, c in self.coeff.items()},
            _coeff_mul(self.const, k)
        )

    def __rmul__(self, k: Coeff) -> "AffExpr":
        return self * k

    def floordiv(self, divisor: int) -> "AffExpr":
        """
        Quasi-affine floor division: expr // divisor.
        Creates a synthetic variable representing the floordiv result.
        """
        if divisor == 1:
            return self
        # Create synthetic var: (expr)//divisor
        synth_name = f"({self})_div_{divisor}"
        return AffExpr({synth_name: 1}, 0)

    def mod(self, divisor: int) -> "AffExpr":
        """
        Quasi-affine modulo: expr % divisor.
        Creates a synthetic variable representing the mod result.
        """
        if divisor == 1:
            return AffExpr.zero()
        # Create synthetic var: (expr)%divisor
        synth_name = f"({self})_mod_{divisor}"
        return AffExpr({synth_name: 1}, 0)

    def substitute(self, var: str, expr: "AffExpr") -> "AffExpr":
        """
        Substitute var := expr.
        If self = c*var + rest, result = c*expr + rest.
        """
        c = self.coeff.get(var, 0)
        if _coeff_is_zero(c):
            return self
        # Remove var from self
        rest = AffExpr(
            {v: co for v, co in self.coeff.items() if v != var},
            self.const
        )
        return rest + (expr * c)

    def rename(self, mapping: Mapping[str, str]) -> "AffExpr":
        """Rename variables according to mapping."""
        new_coeff: Dict[str, Coeff] = {}
        for v, c in self.coeff.items():
            new_v = mapping.get(v, v)
            new_coeff[new_v] = _coeff_add(new_coeff.get(new_v, 0), c)
        return AffExpr(new_coeff, self.const)

    def __str__(self) -> str:
        terms: List[str] = []
        for v in sorted(self.coeff.keys()):
            c = self.coeff[v]
            if _coeff_is_zero(c):
                continue
            if _coeff_is_one(c):
                terms.append(v)
            elif _coeff_is_neg_one(c):
                terms.append(f"-{v}")
            elif isinstance(c, int):
                terms.append(f"{c}*{v}")
            else:
                # Symbolic coefficient
                terms.append(f"({c})*{v}")

        if not _coeff_is_zero(self.const):
            if isinstance(self.const, int):
                terms.append(str(self.const))
            else:
                terms.append(f"({self.const})")

        if not terms:
            return "0"

        result = terms[0]
        for t in terms[1:]:
            if t.startswith("-"):
                result += f" - {t[1:]}"
            else:
                result += f" + {t}"
        return result


# =============================================================================
# Constraint: Equality constraint (expr == 0)
# =============================================================================
@dataclass(frozen=False, eq=False)
class Constraint:
    """
    Equality constraint: expr == 0

    Represents constraints like:
        1500*gid0 + 30*gid1 + gid2 - addr = 0
    """
    expr: AffExpr

    def __hash__(self) -> int:
        return hash(self.expr)

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Constraint):
            return False
        return self.expr == other.expr

    def substitute(self, var: str, aff: AffExpr) -> "Constraint":
        return Constraint(self.expr.substitute(var, aff))

    def rename(self, mapping: Mapping[str, str]) -> "Constraint":
        return Constraint(self.expr.rename(mapping))

    def is_trivial(self) -> bool:
        """Check if constraint is 0 = 0 (always satisfied)."""
        return self.expr.is_zero()

    def is_contradiction(self) -> bool:
        """Check if constraint is const = 0 where const != 0."""
        if self.expr.coeff:
            return False
        return not _coeff_is_zero(self.expr.const)

    def variables(self) -> FrozenSet[str]:
        return self.expr.variables()

    def __str__(self) -> str:
        return f"{self.expr} = 0"


def _try_solve_for(constraint: Constraint, var: str) -> Optional[AffExpr]:
    """
    Try to solve constraint for var.
    Returns None if coefficient is not ±1 (would need division).

    If constraint is: c*var + rest = 0
    And c = ±1, then var = -rest/c
    """
    c = constraint.expr.coeff_of(var)
    if _coeff_is_zero(c):
        return None

    # Only solve if coefficient is ±1 (exact integer division)
    if not (_coeff_is_one(c) or _coeff_is_neg_one(c)):
        return None

    # rest = expr - c*var
    rest = AffExpr(
        {v: co for v, co in constraint.expr.coeff.items() if v != var},
        constraint.expr.const
    )

    # If c = 1: var + rest = 0 => var = -rest
    # If c = -1: -var + rest = 0 => var = rest
    if _coeff_is_one(c):
        return -rest
    else:
        return rest


def eliminate_variables(
    constraints: List[Constraint],
    vars_to_elim: Sequence[str]
) -> List[Constraint]:
    """
    Eliminate variables via Fourier-Motzkin style substitution.

    This is a simplified version that only eliminates variables
    with coefficient ±1 in some constraint.
    """
    constraints = list(constraints)

    for var in vars_to_elim:
        # Find a constraint where var has coefficient ±1
        pivot_idx: Optional[int] = None
        solution: Optional[AffExpr] = None

        for i, c in enumerate(constraints):
            sol = _try_solve_for(c, var)
            if sol is not None:
                pivot_idx = i
                solution = sol
                break

        if pivot_idx is None or solution is None:
            # Cannot eliminate this variable with ±1 coefficient
            continue

        # Remove pivot constraint
        constraints.pop(pivot_idx)

        # Substitute into remaining constraints
        constraints = [c.substitute(var, solution) for c in constraints]

    # Remove trivial constraints (0 = 0)
    return [c for c in constraints if not c.is_trivial()]


# =============================================================================
# BasicMap: Affine Relation
# =============================================================================
@dataclass(frozen=False, eq=False)
class BasicMap:
    """
    An affine relation from domain to range, constrained by equalities.

    Represents: { dom_name[dom_vars] -> rng_name[rng_vars] : constraints }

    Example:
        { S[gid0, gid1, gid2] -> [addr] : addr = 1500*gid0 + 30*gid1 + gid2 }

    The constraints are stored as a list of Constraint objects.
    """
    dom_vars: Tuple[str, ...]
    rng_vars: Tuple[str, ...]
    constraints: Tuple[Constraint, ...]
    dom_name: str = "S"
    rng_name: str = ""  # Empty string for anonymous tuple

    def __hash__(self) -> int:
        return hash((
            self.dom_vars,
            self.rng_vars,
            tuple(hash(c) for c in self.constraints),
            self.dom_name,
            self.rng_name,
        ))

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, BasicMap):
            return False
        return (self.dom_vars == other.dom_vars and
                self.rng_vars == other.rng_vars and
                self.constraints == other.constraints and
                self.dom_name == other.dom_name and
                self.rng_name == other.rng_name)

    @staticmethod
    def from_affine(
        dom_vars: Sequence[str],
        rng_vars: Sequence[str],
        rng_exprs: Sequence[AffExpr],
        *,
        dom_name: str = "S",
        rng_name: str = "",
    ) -> "BasicMap":
        """
        Build a single-valued affine map.

        For each (rng_var, rng_expr) pair, creates constraint:
            rng_var - rng_expr = 0

        Example:
            from_affine(["i", "j"], ["addr"], [1500*i + 30*j])
            => { S[i,j] -> [addr] : addr - 1500*i - 30*j = 0 }
        """
        if len(rng_vars) != len(rng_exprs):
            raise ValueError("rng_vars and rng_exprs length mismatch")

        constraints: List[Constraint] = []
        for rv, ex in zip(rng_vars, rng_exprs):
            # rng_var - expr = 0
            constraints.append(Constraint(AffExpr.var(rv) - ex))

        return BasicMap(
            tuple(dom_vars),
            tuple(rng_vars),
            tuple(constraints),
            dom_name,
            rng_name,
        )

    @staticmethod
    def from_access(
        dom_vars: Sequence[str],
        addr_expr: AffExpr,
        *,
        dom_name: str = "S",
    ) -> "BasicMap":
        """
        Create access relation from domain to single address.

        { S[dom_vars] -> [addr] : addr = addr_expr }
        """
        return BasicMap.from_affine(
            dom_vars,
            ("addr",),
            (addr_expr,),
            dom_name=dom_name,
            rng_name="",
        )

    def all_variables(self) -> FrozenSet[str]:
        """All variables mentioned (domain, range, and in constraints)."""
        vars_set: Set[str] = set(self.dom_vars) | set(self.rng_vars)
        for c in self.constraints:
            vars_set |= c.variables()
        return frozenset(vars_set)

    def rename_vars(self, mapping: Mapping[str, str]) -> "BasicMap":
        """Rename variables throughout the map."""
        new_dom = tuple(mapping.get(v, v) for v in self.dom_vars)
        new_rng = tuple(mapping.get(v, v) for v in self.rng_vars)
        new_cons = tuple(c.rename(mapping) for c in self.constraints)
        return BasicMap(new_dom, new_rng, new_cons, self.dom_name, self.rng_name)

    def reverse(self) -> "BasicMap":
        """
        Reverse the relation: swap domain and range.

        { A -> B : C } becomes { B -> A : C }
        """
        return BasicMap(
            self.rng_vars,
            self.dom_vars,
            self.constraints,
            self.rng_name or "S",
            self.dom_name,
        )

    def apply_range(self, other: "BasicMap") -> "BasicMap":
        """
        Compose on range: result = other ∘ self

        If self: X -> Y and other: Y -> Z
        Then result: X -> Z (via intermediate Y)

        Implementation:
        1. Rename intermediate variables to fresh names
        2. Combine constraints from both maps
        3. Eliminate intermediate variables
        """
        if len(self.rng_vars) != len(other.dom_vars):
            raise ValueError(
                f"Range/domain arity mismatch: {len(self.rng_vars)} vs {len(other.dom_vars)}"
            )

        # Create fresh intermediate variable names
        intermediate = tuple(f"__m{i}" for i in range(len(self.rng_vars)))

        # Rename self's range vars to intermediate
        self_renamed = self.rename_vars(
            {v: m for v, m in zip(self.rng_vars, intermediate)}
        )

        # Rename other's domain vars to intermediate
        other_renamed = other.rename_vars(
            {v: m for v, m in zip(other.dom_vars, intermediate)}
        )

        # Combine constraints
        all_constraints = list(self_renamed.constraints) + list(other_renamed.constraints)

        # Eliminate intermediate variables
        final_constraints = eliminate_variables(all_constraints, intermediate)

        return BasicMap(
            self_renamed.dom_vars,
            other_renamed.rng_vars,
            tuple(final_constraints),
            self_renamed.dom_name,
            other_renamed.rng_name,
        )

    def apply_domain(self, other: "BasicMap") -> "BasicMap":
        """
        Apply transformation to domain: result = self ∘ other

        If other: X -> Y and self: Y -> Z
        Then result: X -> Z
        """
        if len(other.rng_vars) != len(self.dom_vars):
            raise ValueError(
                f"Range/domain arity mismatch: {len(other.rng_vars)} vs {len(self.dom_vars)}"
            )

        intermediate = tuple(f"__m{i}" for i in range(len(self.dom_vars)))

        self_renamed = self.rename_vars(
            {v: m for v, m in zip(self.dom_vars, intermediate)}
        )
        other_renamed = other.rename_vars(
            {v: m for v, m in zip(other.rng_vars, intermediate)}
        )

        all_constraints = list(self_renamed.constraints) + list(other_renamed.constraints)
        final_constraints = eliminate_variables(all_constraints, intermediate)

        return BasicMap(
            other_renamed.dom_vars,
            self_renamed.rng_vars,
            tuple(final_constraints),
            other_renamed.dom_name,
            self_renamed.rng_name,
        )

    def is_empty(self) -> bool:
        """
        Check if the relation is empty (constraints are contradictory).

        This is a conservative check - returns True only if we can prove emptiness.
        """
        for c in self.constraints:
            if c.is_contradiction():
                return True
        return False

    def is_identity(self) -> bool:
        """
        Check if this is an identity relation on matching variables.

        { S[i,j] -> S[i,j] } is identity if dom_vars == rng_vars
        and constraints enforce equality.
        """
        if len(self.dom_vars) != len(self.rng_vars):
            return False

        # Check if each pair has constraint enforcing equality
        # This is a simplified check
        for d, r in zip(self.dom_vars, self.rng_vars):
            if d == r:
                continue
            # Need constraint d - r = 0 or similar
            found = False
            for c in self.constraints:
                expr = c.expr
                if (len(expr.coeff) == 2 and
                    _coeff_is_one(expr.coeff_of(d)) and
                    _coeff_is_neg_one(expr.coeff_of(r)) and
                    _coeff_is_zero(expr.const)):
                    found = True
                    break
            if not found:
                return False
        return True

    def get_dependency_constraint(self) -> Optional[Constraint]:
        """
        If this map represents a dependency (same domain/range structure),
        return the constraint relating primed to unprimed variables.

        Useful for analyzing if fusion is legal.
        """
        if len(self.constraints) == 1:
            return self.constraints[0]
        return None

    def __str__(self) -> str:
        dom = ", ".join(self.dom_vars)
        rng = ", ".join(self.rng_vars)

        dom_str = f"{self.dom_name}[{dom}]" if self.dom_name else f"[{dom}]"
        rng_str = f"{self.rng_name}[{rng}]" if self.rng_name else f"[{rng}]"

        if self.constraints:
            cons_str = " and ".join(str(c) for c in self.constraints)
            return f"{{ {dom_str} -> {rng_str} : {cons_str} }}"
        else:
            return f"{{ {dom_str} -> {rng_str} }}"

    def pretty_str(self) -> str:
        """
        Pretty print with solved form when possible.

        Instead of "addr - 1500*gid0 - 30*gid1 - gid2 = 0"
        Shows "addr = 1500*gid0 + 30*gid1 + gid2"
        """
        dom = ", ".join(self.dom_vars)
        rng_parts: List[str] = list(self.rng_vars)

        # Try to solve each constraint for a range variable
        for i, rv in enumerate(self.rng_vars):
            for c in self.constraints:
                sol = _try_solve_for(c, rv)
                if sol is not None:
                    rng_parts[i] = f"{rv} = {sol}"
                    break

        dom_str = f"{self.dom_name}[{dom}]" if self.dom_name else f"[{dom}]"
        rng_str = f"[{', '.join(rng_parts)}]"

        return f"{{ {dom_str} -> {rng_str} }}"


# =============================================================================
# UnionMap: Union of BasicMaps
# =============================================================================
@dataclass
class UnionMap:
    """
    Union of multiple BasicMaps.

    Represents: { map1 ; map2 ; ... }

    Used for collecting all read/write accesses in a statement.
    """
    maps: List[BasicMap] = field(default_factory=list)

    def add(self, m: BasicMap) -> "UnionMap":
        """Add a map to the union."""
        self.maps.append(m)
        return self

    @staticmethod
    def from_maps(maps: Sequence[BasicMap]) -> "UnionMap":
        return UnionMap(list(maps))

    def reverse(self) -> "UnionMap":
        """Reverse all maps in the union."""
        return UnionMap([m.reverse() for m in self.maps])

    def apply_range(self, other: "UnionMap") -> "UnionMap":
        """
        Apply composition to each pair of maps.

        (A ∪ B) ∘ (C ∪ D) = (A∘C) ∪ (A∘D) ∪ (B∘C) ∪ (B∘D)
        """
        result: List[BasicMap] = []
        for m1 in self.maps:
            for m2 in other.maps:
                try:
                    composed = m1.apply_range(m2)
                    if not composed.is_empty():
                        result.append(composed)
                except ValueError:
                    # Arity mismatch - skip this pair
                    continue
        return UnionMap(result)

    def apply_domain(self, other: "UnionMap") -> "UnionMap":
        """Apply transformation to domain of each map."""
        result: List[BasicMap] = []
        for m1 in self.maps:
            for m2 in other.maps:
                try:
                    composed = m1.apply_domain(m2)
                    if not composed.is_empty():
                        result.append(composed)
                except ValueError:
                    continue
        return UnionMap(result)

    def union(self, other: "UnionMap") -> "UnionMap":
        """Union of two UnionMaps."""
        return UnionMap(self.maps + other.maps)

    def is_empty(self) -> bool:
        """Check if all maps are empty."""
        return all(m.is_empty() for m in self.maps) if self.maps else True

    def __str__(self) -> str:
        if not self.maps:
            return "{ }"
        return "{ " + " ; ".join(str(m)[2:-2] for m in self.maps) + " }"


# =============================================================================
# Dependency Analysis
# =============================================================================
@dataclass
class DependencyInfo:
    """
    Result of dependency analysis between two statements.

    Contains:
    - raw: Read-After-Write dependencies (flow dependencies)
    - war: Write-After-Read dependencies (anti-dependencies)
    - waw: Write-After-Write dependencies (output dependencies)
    - fusible: Whether the statements can potentially be fused
    """
    raw: UnionMap  # W1 ; R2^{-1} - who reads what was written
    war: UnionMap  # R1 ; W2^{-1} - who overwrites what was read
    waw: UnionMap  # W1 ; W2^{-1} - who overwrites what was written
    fusible: bool
    reason: str = ""


def analyze_dependencies(
    writes1: UnionMap,
    reads1: UnionMap,
    writes2: UnionMap,
    reads2: UnionMap,
) -> DependencyInfo:
    """
    Analyze data dependencies between two statements.

    Statement 1 has writes1 and reads1.
    Statement 2 has writes2 and reads2.

    For fusion feasibility:
    - RAW: S1 writes, S2 reads => S2 depends on S1
    - WAR: S1 reads, S2 writes => potential anti-dependency
    - WAW: S1 writes, S2 writes => potential output dependency

    Fusion is legal if all dependencies go in the same direction
    (i.e., S1 before S2 consistently).
    """
    # RAW: W1 ∘ R2^{-1} = { (i1, i2) | W1(i1) = R2(i2) }
    raw = writes1.apply_range(reads2.reverse())

    # WAR: R1 ∘ W2^{-1} = { (i1, i2) | R1(i1) = W2(i2) }
    war = reads1.apply_range(writes2.reverse())

    # WAW: W1 ∘ W2^{-1} = { (i1, i2) | W1(i1) = W2(i2) }
    waw = writes1.apply_range(writes2.reverse())

    # Analyze fusibility
    # Fusion is safe if:
    # 1. RAW dependencies are "forward" (i1 <= i2 in some sense)
    # 2. WAR dependencies don't conflict
    # 3. WAW dependencies don't conflict

    # Note: We no longer mark dimension mismatch as non-fusible here
    # because get_fusion_type can detect tiled fusion patterns.
    # The actual fusibility is determined by get_fusion_type.
    fusible = True
    reason = "Dependencies detected - check fusion type for strategy"

    # Check RAW - this is the critical dependency
    for m in raw.maps:
        if not m.is_identity():
            # Non-identity dependencies may still allow fusion (tiled, shift, etc.)
            if len(m.dom_vars) != len(m.rng_vars):
                reason = f"RAW dimension mismatch: {m}"
            else:
                reason = f"Non-identity RAW dependency: {m}"

    return DependencyInfo(
        raw=raw,
        war=war,
        waw=waw,
        fusible=fusible,
        reason=reason,
    )


def can_fuse(dep: DependencyInfo) -> bool:
    """
    Determine if two loop nests can be fused based on dependency analysis.

    For perfect fusion (same iteration space), we need:
    - RAW dependencies to be identity or "forward"
    - WAR and WAW to not create cycles
    """
    return dep.fusible


def is_perfect_fusion(dep: DependencyInfo) -> bool:
    """
    Check if this is a perfect fusion opportunity (identity dependencies).

    Perfect fusion occurs when:
    - All RAW dependencies are identity (same iteration produces/consumes)
    - This allows complete loop merging without any synchronization
    """
    if not dep.fusible:
        return False

    # Check if all RAW dependencies are identity
    for m in dep.raw.maps:
        if not m.is_identity():
            return False
    return True


@dataclass
class TiledFusionInfo:
    """
    Information about tiling required for fusion.

    When producer writes at Out[h,w] and consumer reads at Out[h*S+rh, w*S+rw],
    we need to tile the producer by [S,S] to align with consumer's strided access.
    """
    # Map from producer dim -> (tile_size, reduction_dim)
    # e.g., {"h": (4, "rh"), "w": (4, "rw")}
    tile_dims: Dict[str, Tuple[int, str]]
    # Producer dimensions that don't need tiling (shared with consumer)
    shared_dims: List[str]
    # The constraint that defines the strided relation
    constraint: Optional[Constraint] = None

    def __str__(self) -> str:
        parts = [f"{d}:tile={t}+{r}" for d, (t, r) in self.tile_dims.items()]
        return f"TiledFusion({', '.join(parts)}, shared={self.shared_dims})"


def detect_strided_relation(raw_map: BasicMap) -> Optional[TiledFusionInfo]:
    """
    Detect if a RAW dependency represents a strided access pattern.

    For Conv+Pool style fusion:
    - Producer domain: (n, k, h, w)
    - Consumer domain: (n, k, hp, wp, rh, rw)
    - Constraint: h = S*hp + rh, w = S*wp + rw

    The constraint `128*h - 512*hp - 128*rh - rw + w - 4*wp = 0` tells us:
    - h (coeff 128) matches rh (coeff -128): same magnitude, offset relation
    - hp (coeff -512) = -4 * 128: scaling factor S=4
    - Similarly for w/wp/rw

    Algorithm:
    1. For each producer var p, find consumer var r with matching coefficient magnitude
       (indicates same logical dimension, just offset)
    2. Then find consumer var q where |coeff(q)| = S * |coeff(p)| for some S > 1
       (indicates scaled/tiled dimension)

    Returns TiledFusionInfo if strided pattern detected, None otherwise.
    """
    if len(raw_map.constraints) != 1:
        return None

    constraint = raw_map.constraints[0]
    expr = constraint.expr

    producer_vars = set(raw_map.dom_vars)
    consumer_vars = set(raw_map.rng_vars)

    # Find shared dimensions (same name in both)
    shared = list(producer_vars & consumer_vars)

    # Producer-only and consumer-only vars
    producer_only = producer_vars - consumer_vars
    consumer_only = consumer_vars - producer_vars

    if not producer_only or not consumer_only:
        return None  # No striding if no dimension difference

    # Strategy: Match by coefficient magnitude first
    # For p = S*q + r:
    # - coeff(p) and coeff(r) have same magnitude (just opposite sign)
    # - coeff(q) = S * coeff(p) (scaled by tile size)

    tile_dims: Dict[str, Tuple[int, str]] = {}
    used_consumer_vars: Set[str] = set()

    # Sort producer vars by coefficient magnitude (largest first) for determinism
    # and to ensure we match "major" dimensions first
    producer_list = sorted(
        [pv for pv in producer_only if isinstance(expr.coeff_of(pv), int) and expr.coeff_of(pv) != 0],
        key=lambda v: abs(expr.coeff_of(v)),
        reverse=True
    )

    for pvar in producer_list:
        p_coeff = expr.coeff_of(pvar)
        p_mag = abs(p_coeff)

        # Step 1: Find reduction var r with matching magnitude
        best_rvar = None
        for rvar in consumer_only - used_consumer_vars:
            r_coeff = expr.coeff_of(rvar)
            if not isinstance(r_coeff, int):
                continue
            if abs(r_coeff) == p_mag:
                best_rvar = rvar
                break

        if best_rvar is None:
            continue

        # Step 2: Find scaled var q where |coeff(q)| = S * p_mag for some S > 1
        best_qvar = None
        best_tile_size = 0
        for qvar in consumer_only - used_consumer_vars - {best_rvar}:
            q_coeff = expr.coeff_of(qvar)
            if not isinstance(q_coeff, int) or q_coeff == 0:
                continue

            q_mag = abs(q_coeff)
            if q_mag > p_mag and q_mag % p_mag == 0:
                tile_size = q_mag // p_mag
                if tile_size > best_tile_size:
                    best_qvar = qvar
                    best_tile_size = tile_size

        if best_qvar is not None and best_tile_size > 1:
            # Found a valid strided pattern
            tile_dims[pvar] = (best_tile_size, best_rvar)
            used_consumer_vars.add(best_qvar)
            used_consumer_vars.add(best_rvar)

    if tile_dims:
        return TiledFusionInfo(
            tile_dims=tile_dims,
            shared_dims=shared,
            constraint=constraint,
        )

    return None


def get_fusion_type(dep: DependencyInfo) -> Tuple[str, Optional[TiledFusionInfo]]:
    """
    Classify the type of fusion possible based on dependencies.

    Returns tuple of:
    - fusion_type: "perfect", "tiled", "shift", "partial", "none"
    - tiling_info: TiledFusionInfo if tiled fusion, None otherwise

    Fusion types:
    - "perfect": Identity dependencies, loops can be completely merged
    - "tiled": Strided access pattern, requires tiling producer to align with consumer
    - "shift": Dependencies have constant offset, can fuse with prologue/epilogue
    - "partial": Some dependencies allow fusion, others require synchronization
    - "none": Fusion not possible
    """
    if dep.raw.is_empty() and dep.war.is_empty() and dep.waw.is_empty():
        return "perfect", None  # No dependencies at all

    # Check for identity dependencies
    all_identity = True
    for m in dep.raw.maps:
        if not m.is_identity():
            all_identity = False
            break

    if all_identity:
        return "perfect", None

    # Check for strided/tiled dependencies (Conv+Pool pattern)
    for m in dep.raw.maps:
        if len(m.dom_vars) != len(m.rng_vars):
            # Dimension mismatch - check for strided pattern
            tiling_info = detect_strided_relation(m)
            if tiling_info:
                return "tiled", tiling_info

    # If dimension mismatch without strided pattern, fusion not possible
    for m in dep.raw.maps:
        if len(m.dom_vars) != len(m.rng_vars):
            return "none", None

    # Check for shift (constant offset) dependencies
    # TODO: Implement shift detection

    return "partial", None


# =============================================================================
# Fusion Transform
# =============================================================================
@dataclass
class FusionResult:
    """Result of attempting to fuse two computation regions."""
    success: bool
    fusion_type: str  # "perfect", "tiled", "shift", "partial", "none"
    dep_info: DependencyInfo
    message: str = ""

    # If fusion succeeded, these contain the merged information
    merged_dom_vars: Optional[Tuple[str, ...]] = None
    merged_read_maps: Optional[List[BasicMap]] = None
    merged_write_maps: Optional[List[BasicMap]] = None

    # For tiled fusion, contains tiling strategy
    tiling_info: Optional[TiledFusionInfo] = None


def attempt_fusion(
    producer_writes: UnionMap,
    producer_reads: UnionMap,
    consumer_writes: UnionMap,
    consumer_reads: UnionMap,
) -> FusionResult:
    """
    Attempt to fuse producer and consumer computations.

    This is the main entry point for fusion analysis and transformation.

    Args:
        producer_writes: Write access relations of the producer
        producer_reads: Read access relations of the producer
        consumer_writes: Write access relations of the consumer
        consumer_reads: Read access relations of the consumer

    Returns:
        FusionResult with success status and merged information if successful
    """
    # Analyze dependencies
    dep_info = analyze_dependencies(
        producer_writes, producer_reads,
        consumer_writes, consumer_reads,
    )

    fusion_type, tiling_info = get_fusion_type(dep_info)

    if fusion_type == "none":
        return FusionResult(
            success=False,
            fusion_type=fusion_type,
            dep_info=dep_info,
            message=dep_info.reason,
        )

    # For perfect fusion, merge the access maps
    if fusion_type == "perfect":
        # Combine read and write maps
        merged_reads = producer_reads.maps + consumer_reads.maps
        merged_writes = producer_writes.maps + consumer_writes.maps

        # Determine merged domain
        # For perfect fusion, domains should match
        merged_dom = None
        for m in merged_reads + merged_writes:
            if merged_dom is None:
                merged_dom = m.dom_vars
            elif m.dom_vars != merged_dom:
                # Domain mismatch - can still do partial fusion
                fusion_type = "partial"
                break

        return FusionResult(
            success=True,
            fusion_type=fusion_type,
            dep_info=dep_info,
            message="Perfect fusion possible - identity dependencies",
            merged_dom_vars=merged_dom,
            merged_read_maps=merged_reads,
            merged_write_maps=merged_writes,
        )

    # For tiled fusion, include tiling strategy
    if fusion_type == "tiled" and tiling_info:
        tile_desc = ", ".join(
            f"{dim}=[{size}x{size}]"
            for dim, (size, _) in tiling_info.tile_dims.items()
        )
        return FusionResult(
            success=True,
            fusion_type=fusion_type,
            dep_info=dep_info,
            message=f"Tiled fusion possible - tile producer dims: {tile_desc}",
            tiling_info=tiling_info,
        )

    # Partial fusion
    return FusionResult(
        success=True,
        fusion_type=fusion_type,
        dep_info=dep_info,
        message=f"Partial fusion possible - {fusion_type} dependencies",
    )


# =============================================================================
# Builder functions for creating maps from IR
# =============================================================================
def build_access_map_from_load(
    load_node: "ATenOp",
    dom_dim: int,
) -> BasicMap:
    """
    Build access relation from a Load IR node.

    Load(Memory, Aff1, Aff2, ...) where each Aff contains
    stride, range, offset, incf information.

    The linearized address is: Σ stride_i * (incf_i * gid_i + offset_i)
    """
    from caten.ir import Aff as IRAff, Const, Range

    dom_vars = tuple(f"gid{i}" for i in range(dom_dim))

    # Build address expression from Aff nodes
    addr_expr = AffExpr.zero()

    for aff in load_node.args[1:]:  # Skip first arg (Memory)
        if not isinstance(aff, IRAff):
            continue

        stride, range_node, offset, incf = aff.args

        # Get dimension from Range
        if isinstance(range_node, Range):
            dim = range_node.dim
            gid_var = f"gid{dim}"
        else:
            continue

        # Build: stride * (incf * gid + offset)
        # For simplicity with symbolic, we track the contribution
        stride_val = stride.item if hasattr(stride, 'item') else stride
        offset_val = offset.item if hasattr(offset, 'item') else offset
        incf_val = incf.item if hasattr(incf, 'item') else incf

        # Create the affine term for this dimension
        if isinstance(stride_val, (int, float)) and isinstance(incf_val, (int, float)):
            coeff = int(stride_val * incf_val)
            const_contrib = int(stride_val * offset_val) if isinstance(offset_val, (int, float)) else 0
        else:
            # Symbolic - use ATenOp
            coeff = _coeff_mul(stride_val, incf_val)
            const_contrib = _coeff_mul(stride_val, offset_val)

        term = AffExpr({gid_var: coeff}, 0)
        addr_expr = addr_expr + term
        if not _coeff_is_zero(const_contrib):
            addr_expr = addr_expr + const_contrib

    return BasicMap.from_access(dom_vars, addr_expr)


# =============================================================================
# Test/Demo
# =============================================================================
if __name__ == "__main__":
    # Example: access pattern for A[1500*i + 30*j + k]
    gid0, gid1, gid2 = "gid0", "gid1", "gid2"

    # Build expression: 1500*gid0 + 30*gid1 + gid2
    addr_expr = 1500 * AffExpr.var(gid0) + 30 * AffExpr.var(gid1) + AffExpr.var(gid2)

    # Create read and write access maps
    r = BasicMap.from_access((gid0, gid1, gid2), addr_expr, dom_name="S")
    w = BasicMap.from_access((gid0, gid1, gid2), addr_expr, dom_name="S")

    print("Read map:")
    print(f"  {r}")
    print(f"  Pretty: {r.pretty_str()}")

    print("\nWrite map:")
    print(f"  {w}")

    # Reverse the read map (for dependency computation)
    r_rev = r.reverse()
    # Rename variables to avoid collision
    r_rev = r_rev.rename_vars({gid0: "gid0p", gid1: "gid1p", gid2: "gid2p"})

    print("\nReversed read map (renamed):")
    print(f"  {r_rev}")

    # Compute dependency: w.apply_range(r_rev)
    d1 = w.apply_range(r_rev)

    print("\nDependency (w ; r^{-1}):")
    print(f"  {d1}")
    print(f"  Pretty: {d1.pretty_str()}")

    # Same with UnionMap
    print("\n--- UnionMap test ---")
    reads = UnionMap.from_maps([r])
    writes = UnionMap.from_maps([w])

    dep = analyze_dependencies(writes, reads, writes, reads)
    print(f"RAW: {dep.raw}")
    print(f"Fusible: {dep.fusible}")
    print(f"Reason: {dep.reason}")
