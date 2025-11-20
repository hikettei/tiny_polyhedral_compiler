from caten.isl import *

try:
    with context():
        print("Context entered")
        bs = BasicSet("{ [i] : 0 <= i <= 1 }")
        print(f"BS created: {bs}")
        print(f"BS handle: {bs.handle}")
        d = bs.dim(ISLDimType.ISL_DIM_SET)
        print(f"Dim: {d}")
except Exception as e:
    print(f"Caught exception: {e}")
