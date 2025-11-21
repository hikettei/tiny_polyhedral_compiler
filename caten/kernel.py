from __future__ import annotations

import inspect
import os
from functools import wraps
from typing import Any, Callable, List, Tuple, Union

from .ops import BinaryOps, ControlOps, MetaOps, Node
from .tensor import DType, Tensor, TensorSpec, f32, float32, i32, int32
from .trace import get_builder


# --- Symbols ---
class Symbol:
    def __init__(self, name: str): self.name = name
    def __repr__(self) -> str: return self.name
    
    def __lt__(self, other: Any) -> Node:
        from .ops import _to_node as ops_to_node
        self_node = Node(MetaOps.VAR, (), arg=self)
        other_node = ops_to_node(other)
        return Node(BinaryOps.LT, (self_node, other_node))

def vars(names: str) -> Tuple[Symbol, ...]:
    return tuple(Symbol(n) for n in names.split())

# --- Directives ---
class Directive:
    def __init__(self, name: str, args: Tuple[Any, ...] = ()):
        self.name = name
        self.args = args
    def __repr__(self) -> str: return f"Directive({self.name})"

def parallel() -> Directive: return Directive("parallel")
def vectorize(width: int = 4) -> Directive: return Directive("vectorize", (width,))
def unroll(factor: int = 4) -> Directive: return Directive("unroll", (factor,))

# --- Range ---
_range_counter = 0

class RangeContext:
    def __init__(self, *args: Union[int, Symbol]):
        global _range_counter
        self.args = args
        self.iter_sym = Symbol(f"i{_range_counter}")
        self.directives: List[Directive] = []
        _range_counter += 1
    
    def __or__(self, other: Directive) -> 'RangeContext':
        self.directives.append(other)
        return self

    def __enter__(self) -> Symbol:
        get_builder().push_block()
        return self.iter_sym
    
    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        body_block = get_builder().pop_block()
        # arg structure: (iter_sym, bounds, body, directives)
        node = Node(ControlOps.RANGE, (), arg=(self.iter_sym, self.args, body_block, self.directives), name=self.iter_sym.name)
        get_builder().push(node)

def range(*args: Union[int, Symbol]) -> RangeContext:
    return RangeContext(*args)

# --- Control Flow ---
class WhenContext:
    def __init__(self, cond: Any):
        self.cond = cond
        
    def __enter__(self) -> None:
        get_builder().push_block()
        
    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        body_block = get_builder().pop_block()
        # arg structure: (cond, then_block, else_block)
        # For now else_block is empty
        node = Node(ControlOps.IF, (), arg=(self.cond, body_block, []))
        get_builder().push(node)

def when(cond: Any) -> WhenContext:
    return WhenContext(cond)

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
            if node.op == ControlOps.RANGE:
                # (iter_sym, bounds, body, directives)
                directives = node.arg[3]
                if directives:
                    print(f"{prefix}  Directives: {directives}")
                print(f"{prefix}  Body:")
                self._print_block(node.arg[2], indent + 2)
            elif node.op == ControlOps.IF:
                print(f"{prefix}  Then:")
                self._print_block(node.arg[1], indent + 2)
                if node.arg[2]:
                    print(f"{prefix}  Else:")
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
            
            # 2. Create Placeholders
            sig = inspect.signature(func)
            func_args = []
            
            if args:
                for arg in args:
                    if isinstance(arg, Tensor):
                        func_args.append(arg)
                        if arg.node.op == MetaOps.PLACEHOLDER:
                             if arg.node not in builder.inputs:
                                 builder.register_input(arg.node)
                    else:
                        func_args.append(arg)
            else:
                for name, param in sig.parameters.items():
                    if isinstance(param.annotation, TensorSpec):
                        node = Node(MetaOps.PLACEHOLDER, (), arg=param.annotation, name=name)
                        builder.register_input(node)
                        func_args.append(Tensor(node))
            
            # 3. Execute Function (Tracing)
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
    "vars", "range", "when", "parallel", "vectorize", "unroll",
    "kernel", "Tensor", "TensorSpec",
    "float32", "int32", "f32", "i32",
    "DType"
]