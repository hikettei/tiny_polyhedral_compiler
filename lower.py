"""
Test AccessMap and polyhedral fusion implementation.

Tests for the unified fusion framework:
1. Reshape+Reshape fusion
2. Matmul+Elementwise fusion  
3. Basic AccessMap functionality
"""
import caten as C
import caten.ir as ir

graph = C.Tensor([10, 10]).reshape(5, 2, 5, 2).sin().reshape(10, 10).sin()

print("Reshape+Reshape")
print(graph.op.viz())

A = C.Tensor([20, 30])
B = C.Tensor([30, 50])
gemm = A @ B
result = gemm.sin()

print("matmul+elwise")
print(result.op.viz())
print(result.render(result.op))
N, C_in, C_out, H, W = 2, 3, 8, 19, 19
KH, KW = 4, 4

x = C.Tensor([N, C_in, H, W])
weight = C.Tensor([C_out, C_in, KH, KW])  # 4x4 conv kernel

# Conv (4x4) -> Sin
conv_out = x.conv2d(weight).pool2d((4, 4))
result_conv = conv_out.sin()

print("Conv+Pool+Elwise Fusion")
print(result_conv.viz())

## Softmax
batch, seq_len, d_k, d_v = 2, 8, 16, 16

Q = C.Tensor([batch, seq_len, d_k])
K = C.Tensor([batch, seq_len, d_k])
V = C.Tensor([batch, seq_len, d_v])

# For simplicity, we'll do 2D attention without batch dimension
# Q @ K^T -> [seq_len, seq_len] -> softmax -> @ V -> [seq_len, d_v]
Q_2d = C.Tensor([seq_len, d_k])
K_2d = C.Tensor([d_k, seq_len])  # Already transposed
V_2d = C.Tensor([seq_len, d_v])
# Attention score: Q @ K^T
scores = Q_2d @ K_2d # [seq_len, seq_len]
# Softmax on last axis
attn_weights = scores.softmax(axis=-1)  # [seq_len, seq_len]
# Output: attention @ V
output = attn_weights @ V_2d  # [seq_len, d_v]
# Lower and check
print("FlashAttention")
print(output.op.viz())
print(output.render(output.op))
