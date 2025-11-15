from __future__ import annotations

import functools
import inspect
from dataclasses import dataclass
from typing import Any, Callable, Dict, Mapping, Optional

from .context import ISLContext
from .obj import ISLObject
from .qualifier import Give, Keep, Null, Param, Qualifier, Take


# ----------------------------------------------------------------------
# Function registry + decorator
# ----------------------------------------------------------------------


@dataclass
class ArgumentSpec:
    name: str
    qualifier: Optional[Qualifier]


@dataclass
class FunctionSpec:
    python_name: str
    primitive_name: str
    signature: inspect.Signature
    arguments: tuple[ArgumentSpec, ...]
    return_spec: Optional[Qualifier]

    def describe(self) -> str:
        arg_desc = ", ".join(
            f"{arg.name}:{arg.qualifier.describe() if arg.qualifier else 'param'}" for arg in self.arguments
        )
        return f"{self.python_name}({arg_desc}) -> {self.return_spec.describe() if self.return_spec else 'param'}"


class ISLFunction:
    """Decorator-centric registry for high-level ISL wrappers."""

    _registry: Dict[str, FunctionSpec] = {}

    @classmethod
    def redefine(cls, primitive_name: str) -> Callable[[Callable[..., Any]], Callable[..., Any]]:
        def decorator(fn: Callable[..., Any]) -> Callable[..., Any]:
            spec = cls._build_spec(fn, primitive_name)
            cls._registry[spec.python_name] = spec

            @functools.wraps(fn)
            def wrapper(*args: Any, **kwargs: Any) -> Any:
                ctx = ISLContext.current(required=True)
                bound = spec.signature.bind(*args, **kwargs)
                bound.apply_defaults()
                prepared = cls._prepare_arguments(spec, bound.arguments, ctx)
                call_args, call_kwargs = cls._reconstruct_call(spec.signature, prepared)
                result = fn(*call_args, **call_kwargs)
                if spec.return_spec is not None:
                    result = spec.return_spec.wrap(result, ctx=ctx)
                return result

            wrapper.__isl_spec__ = spec  # type: ignore[attr-defined]
            return wrapper

        return decorator

    @classmethod
    def get_spec(cls, name: str) -> FunctionSpec:
        return cls._registry[name]

    @classmethod
    def register(cls, primitive_name: str) -> Callable[[Callable[..., Any]], Callable[..., Any]]:
        """Alias for :meth:`redefine` to match the Lisp reference terminology."""

        return cls.redefine(primitive_name)

    # ------------------------------------------------------------------
    @staticmethod
    def _build_spec(fn: Callable[..., Any], primitive_name: str) -> FunctionSpec:
        signature = inspect.signature(fn)
        annotations = dict(fn.__annotations__)
        arg_specs = []
        for param in signature.parameters.values():
            qualifier = _coerce_annotation(annotations.get(param.name))
            arg_specs.append(ArgumentSpec(name=param.name, qualifier=qualifier))
        return_spec = _coerce_annotation(annotations.get("return"))
        return FunctionSpec(
            python_name=fn.__name__,
            primitive_name=primitive_name,
            signature=signature,
            arguments=tuple(arg_specs),
            return_spec=return_spec,
        )

    @staticmethod
    def _prepare_arguments(spec: FunctionSpec, arguments: Mapping[str, Any], ctx: ISLContext) -> Dict[str, Any]:
        prepared: Dict[str, Any] = {}
        for arg in spec.arguments:
            value = arguments[arg.name]
            qualifier = arg.qualifier
            if qualifier is not None:
                value = qualifier.prepare(value, ctx=ctx, name=arg.name)
            prepared[arg.name] = value
        # Include varargs/varkw if present in bound arguments
        for name, value in arguments.items():
            if name not in prepared:
                prepared[name] = value
        return prepared

    @staticmethod
    def _reconstruct_call(signature: inspect.Signature, prepared: Mapping[str, Any]) -> tuple[list[Any], Dict[str, Any]]:
        args: list[Any] = []
        kwargs: Dict[str, Any] = {}
        for param in signature.parameters.values():
            value = prepared[param.name]
            if param.kind in (param.POSITIONAL_ONLY, param.POSITIONAL_OR_KEYWORD):
                args.append(value)
            elif param.kind is param.VAR_POSITIONAL:
                args.extend(value)
            elif param.kind is param.KEYWORD_ONLY:
                kwargs[param.name] = value
            elif param.kind is param.VAR_KEYWORD:
                kwargs.update(value)
        return args, kwargs


def _coerce_annotation(annotation: Any) -> Optional[Qualifier]:
    if annotation is None or annotation is inspect._empty:
        return None
    if isinstance(annotation, Qualifier):
        return annotation
    if isinstance(annotation, type) and issubclass(annotation, ISLObject):
        return Keep(annotation)
    if annotation is type(None):
        return Null()
    return Param(annotation)
