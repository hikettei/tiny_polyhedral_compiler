import caten as C
import caten.ir as ir


def check_shape(tensor, expected):
    # tensor.shape returns tuple[ATenOp], we need to extract values
    # ATenOp.item returns the value if constant, else the op itself
    shape = [s.item if isinstance(s, ir.ATenOp) else s for s in tensor.shape]
    assert tuple(shape) == tuple(expected)

def test_basic_ops():
    # shapes must be compatible. Using shape (3,)
    a = C.Tensor([3], dtype=C.float32)
    b = C.Tensor([3], dtype=C.float32)
    
    c = a + b
    check_shape(c, (3,))
    assert isinstance(c.op, ir.Add)

    d = a - b
    check_shape(d, (3,))
    assert isinstance(d.op, ir.Add) # sub -> add(neg)
    
    e = a * b
    assert isinstance(e.op, ir.Mul)
    
    f = a / b
    assert isinstance(f.op, ir.Mul) # div -> mul(recip)

def test_where_broadcasting():
    # cond: (10, 10)
    cond = C.Tensor([10, 10], dtype=C.float32) 
    # x: (10, 1), y: (1, 10) -> broadcast to (10, 10)
    x = C.Tensor([10, 1], dtype=C.float32)
    y = C.Tensor([1, 10], dtype=C.float32)
    
    res = cond.where(x, y)
    check_shape(res, (10, 10))
    assert isinstance(res.op, ir.Where)

def test_matmul_2d():
    A = C.Tensor([3, 4], dtype=C.float32)
    B = C.Tensor([4, 5], dtype=C.float32)
    C_mat = A @ B
    check_shape(C_mat, (3, 5))
    assert isinstance(C_mat.op, ir.Reduce)

def test_matmul_3d():
    # Batch matmul: (B, M, K) @ (B, K, N) -> (B, M, N)
    B, M, K, N = 2, 3, 4, 5
    A = C.Tensor([B, M, K], dtype=C.float32)
    B_tens = C.Tensor([B, K, N], dtype=C.float32)
    C_mat = A @ B_tens
    check_shape(C_mat, (B, M, N))
    
    # Broadcast on batch dim: (1, M, K) @ (B, K, N) -> (B, M, N)
    A2 = C.Tensor([1, M, K], dtype=C.float32)
    C_mat2 = A2 @ B_tens
    check_shape(C_mat2, (B, M, N))

def test_matmul_4d():
    # (B1, B2, M, K) @ (B1, B2, K, N) -> (B1, B2, M, N)
    B1, B2, M, K, N = 2, 3, 4, 5, 6
    A = C.Tensor([B1, B2, M, K], dtype=C.float32)
    B_tens = C.Tensor([B1, B2, K, N], dtype=C.float32)
    C_mat = A @ B_tens
    check_shape(C_mat, (B1, B2, M, N))

    # Mixed broadcasting
    # (B1, 1, M, K) @ (1, B2, K, N) -> (B1, B2, M, N)
    A_tens_2 = C.Tensor([B1, 1, M, K], dtype=C.float32)
    B_tens_2 = C.Tensor([1, B2, K, N], dtype=C.float32)
    C_mat2 = A_tens_2 @ B_tens_2
    check_shape(C_mat2, (B1, B2, M, N))