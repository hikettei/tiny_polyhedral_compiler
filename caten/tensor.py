from __future__ import annotations
from abc import ABCMeta, abstractmethod
from typing import Any, Optional, Tuple, Union

import caten.ir as ir
# [TODO]
# - Tensor => Fused Tensor Graph Construction
# - Tensor Kernel Construction
# - Then, start working on auto scheduler
DEVICE_TO_TENSOR = {}

class ATenSpec:
    """
    Usage: C.Tensor[float32, "M", "N"] -> TensorSpec(M N)
    """
    def __init__(self, shape: Tuple[Any, ...], dtype: Any = None):
        self.shape = shape
        self.dtype = dtype
    def __repr__(self) -> str: return f"TensorSpec({self.shape}, {self.dtype})"

class ATen:
    op: ATenOp # Just a wrapper for ATenOp
    @classmethod
    def from_shape(cls, shape: List[ATenOp]):
        pass
    
    def apply(self, other: Any, func: Any) -> Tensor:
        other_node = other.node if isinstance(other, Tensor) else other
        res_node = func(self.node, other_node)
        return ATen(node=res_node)
    
    def __class_getitem__(cls, item: Union[Any, Tuple[Any, ...]]) -> TensorSpec:
        # Usage: C.Tensor[10, 10] -> TensorSpec((10, 10))
        pass

    def realize(self):
        pass

class ATenMath():
    def add(self, other):
        pass

class ATenMovements():
    pass

class ATenNN():
    pass

class ATenLinalg():
    pass

class ATenMeta(ATen, ATenMath, ATenNN, ATenMovements, ATenLinalg, metaclass=ABCMeta):
    # Tensor has a shape
    # Tensor has a stride
    # Tensor has a multi level offset
    # Tensor can broadcast
    # Tensor can have a computation graph
    # Can lower
    ## == AbstractionLayer
    @abstractmethod
    def allocate(self):
        pass

    @abstractmethod
    def free(self):
        pass

## For-Style Graph Construction
def kernel(get_kernel: bool = False) -> Callable:
    def decorator(func: Callable) -> Callable:
        pass
    return decorator

class Range():
    pass

class When():
    pass
