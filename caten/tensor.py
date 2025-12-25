from __future__ import annotations
from abc import ABCMeta, abstractmethod
from typing import Any, Optional, Tuple, Union, ClassVar
import os
import caten.ir as ir
from .dtype import float32
# [TODO]
# - Tensor => Fused Tensor Graph Construction
# - Tensor Kernel Construction
# - Then, start working on auto scheduler
## Backend Abstraction
DEVICE_TO_TENSOR = {}
def get_backend(): return os.environ.get("BACKEND", "CPU")
## Annotation
class ATenSpec:
    """
    Usage: C.Tensor[float32, "M", "N"] -> TensorSpec(M N)
    """
    def __init__(self, shape: Tuple[Any, ...], dtype: Any = None):
        self.shape = shape
        self.dtype = dtype
    def __repr__(self) -> str: return f"TensorSpec({self.shape}, {self.dtype})"
## Tensor datastrucure
class ATen:
    op: ATenOp # ATen is just a wrapper for ATenOp
    @classmethod
    def from_shape(cls, shape: List[ATenOp], dtype: DType=float32):
        return ir.Allocate.new(shape, dtype)

    def apply(self, op: Callable, *args: List, **kwargs) -> ATen: return ATen(op=op(*args, **kwargs))
    
    def __class_getitem__(cls, item: Union[Any, Tuple[Any, ...]]) -> TensorSpec:
        # Usage: C.Tensor[10, 10] -> TensorSpec((10, 10))
        # TODO
        pass

    @staticmethod
    def top():
        # Register the given method as @C.sin callable
        pass

    def polyhedral(self):
        pass
## arithmetic mixin
class ATenArith():
    def __add__(self, other):
        pass
## math mixin
class ATenMath():
    pass
## movement ops mixin
class ATenMovements():
    pass
## nn ops mixin
class ATenNN():
    pass
## linalg ops mixin
class ATenLinalg():
    pass
## facet mixin
class Facet():
    # Facet is device transfer abstraction: A.to("CUDA")
    # TODO: with tensor.facet("CUDA") as tensor: ...
    pass
## abstraction over backends
class ATenBase(ATen, ATenMath, ATenNN, ATenMovements, ATenLinalg, metaclass=ABCMeta):
    ## == AbstractionLayer
    @staticmethod
    def register(device_id: str, cls: ClassVar):
        DEVICE_TO_TENSOR[device_id] = cls
    
    @abstractmethod
    def allocate(self):
        pass

    @abstractmethod
    def free(self):
        pass

    @abstractmethod
    def compile(self):
        pass

class Tensor(ATenBase):
    def __new__(cls, *args, **kwargs):
        impl = DEVICE_TO_TENSOR.get(get_backend())
        if impl is None: raise ValueError(f"Unknown BACKEND={get_backend()}")
        return impl(*args, **kwargs)
## For-Style Graph Construction
def kernel(get_kernel: bool = False) -> Callable:
    def decorator(func: Callable) -> Callable:
        pass
    return decorator

# how to generate polyhedral model from tensor ops?
# rangeify -> range/when ==> polyhedral model
# with C.range(10, 10):
# with C.when(10, 10)
class range():
    pass

class when():
    pass
