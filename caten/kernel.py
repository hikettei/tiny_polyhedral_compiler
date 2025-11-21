from __future__ import annotations

import inspect
import os
from functools import wraps
from typing import Any, Callable, List, Tuple, Union

from .ops import Node, OpType
from .tensor import DType, Tensor, TensorSpec, f32, float32, i32, int32
from .trace import get_builder


# --- Symbols ---
class Symbol:
    def __init__(self, name: str): self.name = name
    def __repr__(self) -> str: return self.name

def vars(names: str) -> Tuple[Symbol, ...]:
    return tuple(Symbol(n) for n in names.split())

# --- Range ---
_range_counter = 0

class RangeContext:
    def __init__(self, *args: Union[int, Symbol]):
        global _range_counter
        self.args = args
        # Assign unique name like i0, i1, i2...
        self.iter_sym = Symbol(f"i{_range_counter}")
        _range_counter += 1
    
    def __enter__(self) -> Symbol:
        get_builder().push_block()
        return self.iter_sym
    
    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        body_block = get_builder().pop_block()
        node = Node(OpType.RANGE, (), arg=(self.iter_sym, self.args, body_block), name=self.iter_sym.name)
        get_builder().push(node)

def range(*args: Union[int, Symbol]) -> RangeContext:
    return RangeContext(*args)

# --- Kernel ---
class Kernel:
    def __init__(self, compiled_kernel: Any, graph: List[Node]):
        self.compiled_kernel = compiled_kernel
        self.graph = graph
    
    def __call__(self, *args: Any, **kwargs: Any) -> Any:
        return self.compiled_kernel(*args, **kwargs)
    
    def print_graph(self) -> None:
        print("--- Execution Graph ---")
        self._print_block(self.graph, 0)

    def _print_block(self, block: List[Node], indent: int) -> None:
        prefix = "  " * indent
        for node in block:
            print(f"{prefix}{node}")
            if node.op == OpType.RANGE:
                print(f"{prefix}  Body:")
                self._print_block(node.arg[2], indent + 2)

def kernel(get_kernel: bool = False) -> Callable:
    def decorator(func: Callable) -> Callable:
        @wraps(func)
        def wrapper(*args: Any, **kwargs: Any) -> Any:
            # 1. Setup
            global _range_counter
            _range_counter = 0
            builder = get_builder()
            builder.reset()
            
            # 2. Create Placeholders from annotations if args are missing/mismatched
            sig = inspect.signature(func)
            func_args = []
            
            if args:
                for arg in args:
                    if isinstance(arg, Tensor):
                        func_args.append(arg)
                        if arg.node.op == OpType.PLACEHOLDER:
                             if arg.node not in builder.inputs:
                                 builder.register_input(arg.node)
                    else:
                        func_args.append(arg)
            else:
                for name, param in sig.parameters.items():
                    if isinstance(param.annotation, TensorSpec):
                        node = Node(OpType.PLACEHOLDER, (), arg=param.annotation, name=name)
                        builder.register_input(node)
                        func_args.append(Tensor(node))
            
            # 3. Execute Function (Tracing)
            # The return value is currently ignored as we build the graph via side effects
            _ = func(*func_args)
            
            # 4. Finalize Graph
            full_graph = builder.root_block
            
            # 5. Compile
            runtime_name = os.environ.get("RUNTIME", "CLANG")
            if runtime_name == "CLANG":
                from .runtimes.clang import ClangRuntime
                runtime = ClangRuntime()
            else:
                raise NotImplementedError(f"Runtime {runtime_name} not supported")
                
            compiled = runtime.compile(full_graph, builder.inputs)
            
            k_obj = Kernel(compiled, full_graph)
            
            if get_kernel:
                return k_obj
            return k_obj(*args)

        return wrapper
    return decorator

__all__ = [
    "vars", "range", "kernel", "Tensor", "TensorSpec",
    "float32", "int32", "f32", "i32",
    "DType"
]