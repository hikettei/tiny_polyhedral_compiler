from __future__ import annotations

import ctypes
import os
from ctypes import CDLL
from ctypes.util import find_library
from pathlib import Path
from typing import Optional

# ``ctypes`` exposes libisl handles as plain Python integers (void * as int).
FfiPointer = int
_LIB: Optional[CDLL] = None

def _candidate_paths() -> list[str]:
    env_path = os.getenv("ISL_LIB_PATH")
    candidates = []
    if env_path:
        candidates.append(env_path)
    libname = find_library("isl")
    if libname:
        candidates.append(libname)
    candidates.extend(["libisl.dylib", "libisl.so", "isl.dll"])
    user_usr = Path.home() / "usr"
    for base in list(candidates):
        path = Path(base)
        if not path.is_absolute():
            candidates.append(str(user_usr / base))
    return candidates

def load_libisl() -> CDLL:
    global _LIB
    if _LIB is not None:
        return _LIB
    errors: list[str] = []
    for candidate in _candidate_paths():
        try:
            _LIB = ctypes.CDLL(candidate)
            return _LIB
        except OSError as exc:  # pragma: no cover - best effort
            errors.append(f"{candidate}: {exc}")
    raise RuntimeError(
        "Caten depends on ISL (libisl) but the shared library was not found.\n"
        + "\n".join(errors)
    )
