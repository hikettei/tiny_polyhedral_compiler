from __future__ import annotations

import ctypes
import os
import warnings
from ctypes import CDLL
from ctypes.util import find_library
from pathlib import Path
from typing import Optional

# ``ctypes`` exposes libisl handles as plain Python integers (void * as int).
FfiPointer = int

_LIB: Optional[CDLL] = None
_WARN_MESSAGE = (
    "Caten depends on the Integer Set Library (libisl) but the shared library"
    " could not be located. Please install libisl (e.g., `brew install isl` or"
    " `sudo apt install libisl-dev`) and ensure it is discoverable."
)

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

def load_libisl() -> Optional[CDLL]:
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
    warnings.warn(_WARN_MESSAGE + "\n" + "\n".join(errors), RuntimeWarning)
    _LIB = None
    return None

def require_libisl() -> CDLL:
    lib = load_libisl()
    if lib is None:
        raise RuntimeError("libisl is required but could not be loaded.")
    return lib
