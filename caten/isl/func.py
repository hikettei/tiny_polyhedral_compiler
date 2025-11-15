from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Callable, Dict, Optional, Tuple

from .qualifier import Qualifier


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
        primitive: Callable[..., Any],
        *arg_qualifiers: Qualifier,
        return_: Optional[Qualifier] = None,
    ) -> Callable[..., Any]:
        py_name = getattr(primitive, "__name__", primitive.__class__.__name__)
        spec = FunctionSpec(primitive, tuple(arg_qualifiers), return_)
        wrapper = cls._build_wrapper(py_name, spec)
        cls._registry[py_name] = spec
        return wrapper

    @classmethod
    def get_spec(cls, name: str) -> FunctionSpec:
        return cls._registry[name]

    @staticmethod
    def _build_wrapper(py_name: str, spec: FunctionSpec) -> Callable[..., Any]:
        def wrapper(*user_args: Any) -> Any:
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
            if spec.return_spec is not None:
                result = spec.return_spec.wrap(result, ctx=ctx, name="return")
            return result

        wrapper.__name__ = py_name
        return wrapper
