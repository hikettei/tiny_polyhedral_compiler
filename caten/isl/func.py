from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Callable, Dict, Iterable, Optional, Tuple

from .context import ISLContext
from .qualifier import Qualifier


@dataclass
class FunctionSpec:
    python_name: str
    primitive: Callable[..., Any]
    arguments: Tuple[Qualifier, ...]
    return_spec: Optional[Qualifier]

    def describe(self) -> str:
        arg_desc = ", ".join(arg.describe() for arg in self.arguments)
        ret = self.return_spec.describe() if self.return_spec else "param"
        return f"{self.python_name}({arg_desc}) -> {ret}"


class ISLFunction:
    """Factory for lightweight wrappers around libisl primitives."""

    _registry: Dict[str, FunctionSpec] = {}

    @classmethod
    def create(
        cls,
        primitive: Callable[..., Any],
        python_name: str,
        *arg_qualifiers: Qualifier,
        return_: Optional[Qualifier] = None,
    ) -> Callable[..., Any]:
        spec = FunctionSpec(python_name, primitive, tuple(arg_qualifiers), return_)
        wrapper = cls._build_wrapper(spec)
        wrapper.__name__ = python_name
        cls._registry[python_name] = spec
        return wrapper

    @classmethod
    def get_spec(cls, name: str) -> FunctionSpec:
        return cls._registry[name]

    @staticmethod
    def _build_wrapper(spec: FunctionSpec) -> Callable[..., Any]:
        def wrapper(*user_args: Any) -> Any:
            ctx = ISLContext.current(required=True)
            prepared_args: list[Any] = []
            arg_iter = iter(user_args)
            for index, qualifier in enumerate(spec.arguments):
                if qualifier.requires_argument:
                    try:
                        raw = next(arg_iter)
                    except StopIteration as exc:
                        raise TypeError(
                            f"Missing argument #{index + 1} for {spec.python_name}."
                        ) from exc
                else:
                    raw = None
                prepared_args.append(qualifier.prepare(raw, ctx=ctx, name=f"arg{index}"))
            try:
                next(arg_iter)
            except StopIteration:
                pass
            else:
                raise TypeError(f"Too many arguments for {spec.python_name}.")
            result = spec.primitive(*prepared_args)
            if spec.return_spec is not None:
                result = spec.return_spec.wrap(result, ctx=ctx, name="return")
            return result

        return wrapper
