from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Callable, Dict, Optional, Tuple

from .qualifier import Qualifier
from .ffi import load_libisl

@dataclass(frozen=True)
class FunctionSpec:
    primitive: Callable[..., Any]
    arguments: Tuple[Qualifier, ...]
    return_spec: Optional[Qualifier]

class ISLFunction:
    """Factory for lightweight wrappers around libisl primitives."""

    _registry: Dict[str, FunctionSpec] = {}

    @classmethod
    def create(
        cls,
        primitive: Any,
        *arg_qualifiers: Qualifier,
        return_: Optional[Qualifier] = None,
        lib: Optional[Any] = load_libisl(),
    ) -> Callable[..., Any]:
        assert lib is not None, "ISLFunction requires lib"
        assert return_ is not None, "return_ is required"
        func = cls._resolve_and_configure(primitive, lib, tuple(arg_qualifiers), return_)
        py_name = getattr(func, "__name__", func.__class__.__name__)
        spec = FunctionSpec(func, tuple(arg_qualifiers), return_)
        wrapper = cls._build_wrapper(py_name, spec)
        cls._registry[py_name] = spec
        return wrapper

    @classmethod
    def get_spec(cls, name: str) -> FunctionSpec:
        return cls._registry[name]

    @staticmethod
    def _build_wrapper(py_name: str, spec: FunctionSpec) -> Callable[..., Any]:
        def wrapper(*user_args: Any, return_raw_pointer: bool = False) -> Any:
            from .specs.context import current  # local import to avoid cycles
            ctx = current(required=False)
            prepared_args: list[Any] = []
            arg_iter = iter(user_args)
            for index, qualifier in enumerate(spec.arguments):
                if qualifier.requires_argument:
                    try:
                        raw = next(arg_iter)
                    except StopIteration as exc:
                        raise TypeError(
                            f"Missing argument #{index + 1} for {py_name}."
                        ) from exc
                else:
                    raw = None
                prepared_args.append(qualifier.prepare(raw, ctx=ctx, name=f"arg{index}"))
            try:
                next(arg_iter)
            except StopIteration:
                pass
            else:
                raise TypeError(f"Too many arguments for {py_name}.")
            result = spec.primitive(*prepared_args)
            # ISL returned null_pointer while spec is defined as pointer? => Error
            if spec.return_spec is not None and result is None:
                ctx.raise_isl_error()
            if spec.return_spec is not None and return_raw_pointer is not None:
                result = spec.return_spec.wrap(result, ctx=ctx, name="return")
            return result
        wrapper.__name__ = py_name
        return wrapper

    @staticmethod
    def _resolve_and_configure(
        primitive: Any,
        lib: Optional[Any],
        arguments: Tuple[Qualifier, ...],
        return_spec: Optional[Qualifier],
    ) -> Callable[..., Any]:
        if isinstance(primitive, str):
            if lib is None:
                raise ValueError("lib must be provided when primitive is given by name.")
            primitive = getattr(lib, primitive)
        elif lib is not None:
            primitive = getattr(lib, getattr(primitive, "__name__", primitive))

        primitive.argtypes = [q.as_ctype() for q in arguments]
        primitive.restype = return_spec.as_ctype()                
        return primitive
