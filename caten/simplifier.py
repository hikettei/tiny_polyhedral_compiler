from __future__ import annotations

import inspect, operator
import math

from dataclasses import is_dataclass, replace
from typing import Any, Callable, Dict, List, Optional, Tuple, Type, Union

from .ir import ATenOp
import caten.ir as ir
OpType = Type[ATenOp]

class Pat:
    def __init__(
        self,
        op: Union[OpType, Tuple[OpType, ...], None] = None,
        name: Optional[str] = None,
        src: Optional[Tuple["Pat", ...]] = None,
        meta: Optional[Dict[str, Callable[[ATenOp], Any]]] = None,
    ):
        self.op = op if (op is None or isinstance(op, tuple)) else (op,)
        self.name, self.src = name, src
        self.meta = meta or {}
    def match(self, n: ATenOp, ctx: Dict[str, Any]) -> bool:
        if self.op is not None and not isinstance(n, self.op): return False
        if self.src is not None:
            ks = tuple(n.args)
            if len(ks) != len(self.src): return False
            if not all(p.match(k, ctx) for p, k in zip(self.src, ks, strict=True)): return False
        for var, ex in self.meta.items():
            v = ex(n)
            if var in ctx and ctx[var] != v: return False
            ctx[var] = v
        if self.name:
            if self.name in ctx and ctx[self.name] != n: return False
            ctx[self.name] = n
        return True
    @staticmethod
    def var(name: str) -> "Pat":
        return Pat(name=name)

class Simplifier:
    def __init__(self, patterns: List[Tuple[Pat, Callable[..., Any]]]):
        self.patterns = patterns
    def __add__(self, other: Simplifier) -> Simplifier:
        return Simplifier(self.patterns+other.patterns)
    def rewrite(self, n: ATenOp, ctx_obj: Any = None) -> Optional[ATenOp]:
        for pat, fn in self.patterns:
            m: Dict[str, Any] = {}
            if not pat.match(n, m): continue

            sig = inspect.signature(fn)
            argv = [(ctx_obj if p == "ctx" else m.get(p)) for p in sig.parameters]
            out = fn(*argv)

            if out is None: continue
            if isinstance(out, ATenOp): return out
            raise TypeError(f"rewrite returned unsupported type: {type(out)}")
        return None

    def _walk_once(self, root: ATenOp, ctx_obj: Any = None) -> Tuple[ATenOp, bool]:
        changed = False
        memo: Dict[int, ATenOp] = {}
        def go(n: ATenOp) -> ATenOp:
            nonlocal changed
            if (r := memo.get(id(n))) is not None: return r
            ks = tuple(n.args)
            if ks:
                ks2 = tuple(go(k) for k in ks)
                if ks2 != ks and is_dataclass(n):
                    n = replace(n, args=type(n.args)(ks2))
                    changed = True
            while True:
                r2 = self.rewrite(n, ctx_obj)
                if r2 is None or r2 == n: break
                n = r2
                changed = True
            memo[id(n)] = n
            return n
        return go(root), changed

    def simplify(self, root: ATenOp, ctx_obj: Any = None, max_iters: int = 30000) -> ATenOp:
        cur = root
        for _ in range(max_iters):
            cur, ch = self._walk_once(cur, ctx_obj)
            if not ch: break
        return cur

# Guard Methods
def Guard(obj): pass
def _is_num(x: Any) -> bool:
    return isinstance(x, (int, float)) and not isinstance(x, bool)

constant_folder = Simplifier(
    # UnaryOps
    [(
        Pat(op, src=(Pat(ir.Const, name="x"),)),
        (lambda op: (lambda x: ir.Const.new(op.python_op(x.value), x.T.dtype)
                     if _is_num(x.value) else None))(op),
    ) for op in [ir.Neg, ir.Recip, ir.Sin, ir.Exp2, ir.Log2, ir.Sqrt]]
    +
    # BinaryOps
    [(
        Pat(op, src=(Pat(ir.Const, name="a"), Pat(ir.Const, name="b"))),
        (lambda op: (lambda a, b: ir.Const.new(op.python_op(a.value, b.value), a.T.dtype)
                     if (a.T.dtype == b.T.dtype and _is_num(a.value) and _is_num(b.value)) else None))(op),
    ) for op in [ir.Add, ir.Mul, ir.IDiv, ir.Max, ir.Mod, ir.Neq, ir.Lt]]
)


simplifier = constant_folder
