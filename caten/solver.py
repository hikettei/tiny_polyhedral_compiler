from __future__ import annotations

from dataclasses import dataclass, field
import caten.ir as ir
# [TODO] Whether They should be defined as

@dataclass(frozen=False, eq=False)
class Constraint:
    """Equality constraint: expr == 0"""
    expr: ir.ATenOp
    def __hash__(self) -> int: return hash(self.expr)
    def __eq__(self, other: ir.ATenOp) -> bool: return ir.ATenOp.eql(self.expr, other)
    def substitute(self, name: str, aff: ir.ATenOp) -> "Constraint":
        # TODO: pm
        return Constraint(self.expr.substitute(var, aff))
    def rename(self, mapping: Mapping[str, str]) -> "Constraint":
        # TODO: pm
        return Constraint(self.expr.rename(mapping))
    def is_trivial(self) -> bool: return ir.ATenOp.eql(self.expr, 0)
    def is_contradiction(self) -> bool:
        """Check if constraint is const = 0 where const != 0."""
        # ??
        if self.expr.coeff: return False
        return not _coeff_is_zero(self.expr.const)

    def variables(self) -> FrozenSet[str]: return self.expr.variables()
    def __str__(self) -> str: return f"{self.expr.render()} = 0"

    
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
    rng_name: str = ""
    
@dataclass
class UnionMap:
    """Union of multiple BasicMaps. { map1; map2; ...}"""
    maps: List[BasicMap] = field(default_factory=list)
    def reverse(self) -> UnionMap: return UnionMap([m.reverse() for m in self.maps])
    def is_empty(self) -> bool: return all(m.is_empty() for m in self.maps) if self.maps else True
    def apply_range(self, other: UnionMap) -> UnionMap:
        result: List[BasicMap] = []
        for m1 in self.maps:
            for m2 in other.maps:
                if not (composed:=m1.apply_range(m2)).is_empty():
                    result.append(composed)
        return UnionMap(result)
    def apply_domain(self, other: UnionMap) -> UnionMap:
        result: List[BasicMap] = []
        for m1 in self.maps:
            for m2 in other.maps:
                if not (composed:=m1.apply_domain(m2)).is_empty():
                    result.append(composed)
        return UnionMap(result)
# Exec: UnionMapをDomainとして持っておくべきか？
