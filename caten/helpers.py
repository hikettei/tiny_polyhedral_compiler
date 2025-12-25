from __future__ import annotations

import functools
import operator
from typing import Any, Iterable, TypeVar

T = TypeVar("T")

def prod(x:Iterable[T]) -> T|int: return functools.reduce(operator.mul, x, 1)

def argfix(*x: Any) -> tuple[Any, ...]:
  if x and x[0].__class__ in (tuple, list):
    if len(x) != 1: raise ValueError(f"bad arg {x}")
    return tuple(x[0])
  return x

def align_left(*shapes: tuple[Any, ...]) -> tuple[tuple[Any, ...], ...]:
  # unsqueeze left to make every shape same length
  max_dim = max(len(shape) for shape in shapes)
  return tuple((1,) * (max_dim - len(shape)) + shape for shape in shapes)
