from __future__ import annotations

from dataclasses import dataclass


class DTypeMetaClass(type):
    dcache: dict[tuple, DType] = {}
    def __call__(cls, *args, **kwargs):
        if (ret:=DTypeMetaClass.dcache.get(args, None)) is not None: return ret
        DTypeMetaClass.dcache[args] = ret = super().__call__(*args)
        return ret

# TODO: Vector/Packed DType
@dataclass(frozen=True, eq=False)
class DType:
    name: str
    @staticmethod
    def new(name:str): return DType(name)

## definitions
float64 = DType.new("float64")
float32 = DType.new("float32")
float16 = DType.new("float16")

int64 = DType.new("int64")
int32 = DType.new("int32")
int16 = DType.new("int16")
int8 = DType.new("int8")
uint64 = DType.new("uint64")
uint32 = DType.new("uint32")
uint16 = DType.new("uint16")
uint8 = DType.new("uint8")

## dtype aliases
index = int64
default_float = float32

floats = [float64, float32, float16]
integers = [int64, int32, int16, int8, uint64, uint32, uint16, uint8]
