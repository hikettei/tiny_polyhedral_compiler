from typing import Any, Dict, Type

_TYPES: Dict[str, Type[Any]] = {}

def register_type(name: str, cls: Type[Any]) -> None:
    _TYPES[name] = cls

def resolve_type(name: str) -> Type[Any]:
    if name not in _TYPES:
        raise KeyError(f"Type '{name}' not registered.")
    return _TYPES[name]
