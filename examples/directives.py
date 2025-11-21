import os

import caten as C

os.environ["RUNTIME"] = "CLANG"

N, = C.vars("N")

@C.kernel(get_kernel=True)
def parallel_copy(A: C.Tensor[N], B: C.Tensor[N]):
    # Directive syntax test
    with (C.range(N) | C.parallel()) as i:
        B[i] = A[i]

if __name__ == "__main__":
    print("Compiling Kernel...")
    A = C.Tensor(10, dtype=C.float32, name="A")
    B = C.Tensor(10, dtype=C.float32, name="B")
    
    k = parallel_copy(A, B)
    
    print("\n[Graph Visualization]")
    k.print_graph()
    
    print("\n[Generated Code]")
    k()
