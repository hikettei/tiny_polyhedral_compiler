from typing import Any

import caten as C


class CPUTensor(C.ATen):
    def allocate(self) -> None:
        pass

    def free(self) -> None:
        pass

    #@staticmethod
    def compile(self) -> None:
        pass

    @staticmethod
    def render(op: Any) -> None:
        def _render(node: Any) -> None:
            pass

C.ATen.register("CPU", CPUTensor)
