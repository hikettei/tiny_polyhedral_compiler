import os

import caten as C

# Set Runtime
os.environ["RUNTIME"] = "CLANG"

# Define Symbols
N, M, K = C.vars("N M K")

# Define Kernel
@C.kernel(get_kernel=True)
def matmul(A: C.Tensor[N, K], B: C.Tensor[K, M], Out: C.Tensor[N, M]):
    with C.range(N) as i:
        with C.range(M) as j:
            Out[i, j] = 0.0
            with C.range(K) as k:
                Out[i, j] = Out[i, j] + A[i, k] * B[k, j]

if __name__ == "__main__":
    print("Compiling Kernel...")
    
    A = C.Tensor(10, 10, dtype=C.float32, name="A")
    B = C.Tensor(10, 10, dtype=C.float32, name="B")
    Out = C.Tensor(10, 10, dtype=C.float32, name="Out")
    
    k = matmul(A, B, Out)
    
    print("\n[Graph Visualization]")
    k.print_graph()
    
    print("\n[Generated Code]")
    k()
