from __future__ import annotations
from typing import Iterable, TypeVar
import functools, operator

T = TypeVar("T")

def prod(x:Iterable[T]) -> T|int: return functools.reduce(operator.mul, x, 1)

def argfix(*x):
  if x and x[0].__class__ in (tuple, list):
    if len(x) != 1: raise ValueError(f"bad arg {x}")
    return tuple(x[0])
  return x
