from typing import Any

import caten as C
import caten.ir as ir

class CPUTensor(C.TensorImpl):
    def allocate(self) -> None:
        pass

    def free(self) -> None:
        pass

    def compile(self) -> None:
        pass

    @staticmethod
    def render(op: Any) -> None:
        def _render(node: Any) -> None:
            match node:
                case ir.Add(): return f"{_render(node.args[0])}+{_(render(node.args[0]))}"
                case ir.Mul(): return f"{_render(node.args[0])}+{_(render(node.args[0]))}"
                case ir.Const(): return f"{node.value}"
                case ir.Range(): return f"gid{node.dim}"
                case ir.Aff(): return f"{_render(node.args[0])}*({_render(node.args[3])}*{_render(node.args[1])}+{_render(node.args[2])})"
                case _: raise RuntimeError(f"No rendering rule for {node.__class__}")
        return _render(op)

C.ATen.register("CPU", CPUTensor)
