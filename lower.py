"""
Test AccessMap and FusionEngine implementation.

Tests for the unified fusion framework:
1. Reshape+Reshape fusion
2. Matmul+Elementwise fusion  
3. Basic AccessMap functionality
"""
import caten as C
from caten.dtype import float32
from caten.ir import (
    AccessMap,
    ATenOpType,
    Sync,
    FusionEngine,
    Load,
    Memory,
    Sin,
    Store,
)

print("=" * 60)
print("Test 1: AccessMap Creation")
print("=" * 60)

# Test AccessMap.from_tensor_type
T = ATenOpType.from_shape((10, 20), float32)
access_map = AccessMap.from_tensor_type(T)
print("AccessMap from shape [10, 20]:")
print(f"  n_ranges: {access_map.n_ranges}")
print(f"  dims: {access_map.dims}")
print(f"  domain_shape: {[s for s in access_map.domain_shape]}")

print()
print("=" * 60)
print("Test 2: Reshape Fusion - [10,10] -> [2,5,2,5] -> [10,10]")
print("=" * 60)

# Create reshape chain
a = C.Tensor([10, 10])
b = a.reshape([2, 5, 2, 5])
c = b.reshape([10, 10])
d = c.sin()

print(f"Input shape: {[ax.size for ax in a.op.T.axes]}")
print(f"After reshape 1: {[ax.size for ax in b.op.T.axes]}")
print(f"After reshape 2: {[ax.size for ax in c.op.T.axes]}")
print(f"After sin: {[ax.size for ax in d.op.T.axes]}")

# Lower and check fusion
lowered = d.op.lower()
print(f"\nLowered type: {type(lowered).__name__}")
if isinstance(lowered, Sync):
    print(f"Sync dims: {lowered.dims}")
    print(f"n_dims: {lowered.n_dims}")
    
    # Check iteration domain
    domain = lowered.get_iteration_domain()
    print(f"Iteration domain dims: {domain.dims}")

print()
print()
print("=" * 60)
print("Test 3: FusionEngine Analysis")
print("=" * 60)

# Test domain equality
T1 = ATenOpType.from_shape((10, 20), float32)
T2 = ATenOpType.from_shape((10, 20), float32)
T3 = ATenOpType.from_shape((20, 10), float32)

am1 = AccessMap.from_tensor_type(T1)
am2 = AccessMap.from_tensor_type(T2)
am3 = AccessMap.from_tensor_type(T3)
T3 = ATenOpType.from_shape((20, 10), float32)

am1 = AccessMap.from_tensor_type(T1)
am2 = AccessMap.from_tensor_type(T2)
am3 = AccessMap.from_tensor_type(T3)

print(f"AccessMap [10,20] equals [10,20]: {am1.domain_equals(am2)}")
print(f"AccessMap [10,20] equals [20,10]: {am1.domain_equals(am3)}")

print()
print("=" * 60)
print("Test 4: Matmul + Elementwise")
print("=" * 60)

# GEMM: C[i,j] = sum_k A[i,k] * B[k,j]
# Then: D[i,j] = sin(C[i,j])
A = C.Tensor([20, 30])
B = C.Tensor([30, 50])
gemm = A @ B
result = gemm.sin()

print(f"A shape: {[ax.size for ax in A.op.T.axes]}")
print(f"B shape: {[ax.size for ax in B.op.T.axes]}")
print(f"GEMM shape: {[ax.size for ax in gemm.op.T.axes]}")
print(f"Result shape: {[ax.size for ax in result.op.T.axes]}")

# Lower GEMM
lowered_gemm = gemm.op.lower()
print(f"\nLowered GEMM type: {type(lowered_gemm).__name__}")

# Lower the full expression
lowered_result = result.op.lower()
print(f"Lowered result type: {type(lowered_result).__name__}")

if isinstance(lowered_result, Sync):
    print(f"Result Sync dims: {lowered_result.dims}")
    
    # Check if fusion happened by looking at load sources
    sources = lowered_result.load_sources()
    print(f"Number of kernel boundaries: {len(sources)}")

print()
print("=" * 60)
print("Test 5: Load.get_access_map()")
print("=" * 60)

# Create a simple load and extract its AccessMap
mem = Memory.defglobal((10, 20), float32)
load = Load.from_tensor(mem)
print(f"Load created: {type(load).__name__}")

if isinstance(load, Load):
    am = load.get_access_map()
    print("Extracted AccessMap:")
    print(f"  n_ranges: {am.n_ranges}")
    print(f"  dims: {am.dims}")
    print(f"  domain_shape: {[s for s in am.domain_shape]}")

print()
print("=" * 60)
print("Test 6: Conv + Pool + Elementwise")
print("=" * 60)
# Conv2D (1x1): [N, C_in, H, W] * [C_out, C_in, 1, 1] -> [N, C_out, H, W]
# Pool2D: [N, C_out, H, W] -> [N, C_out, H', W']
# Then apply sin
N, C_in, C_out, H, W = 2, 3, 8, 8, 8
pool_size = 2

x = C.Tensor([N, C_in, H, W])
weight = C.Tensor([C_out, C_in, 1, 1])  # 1x1 conv kernel

# Conv (1x1) -> Pool -> Sin
conv_out = x.conv2d(weight)
pooled = conv_out.pool2d(kernel_size=pool_size, op="max")
result_conv = pooled.sin()

print(f"Input shape: {[ax.size for ax in x.op.T.axes]}")
print(f"Weight shape: {[ax.size for ax in weight.op.T.axes]}")
print(f"After conv2d (1x1): {[ax.size for ax in conv_out.op.T.axes]}")
print(f"After pool2d: {[ax.size for ax in pooled.op.T.axes]}")
print(f"After sin: {[ax.size for ax in result_conv.op.T.axes]}")

lowered_conv = result_conv.op.lower()
print(f"\nLowered conv+pool+sin type: {type(lowered_conv).__name__}")
if isinstance(lowered_conv, Sync):
    print(f"Sync dims: {lowered_conv.dims}")
    sources = lowered_conv.load_sources()
    print(f"Kernel boundaries: {len(sources)}")
    # Print IR graph
    print("\n--- IR Graph ---")
    print(lowered_conv.viz())
print()
print("=" * 60)
print("Test 7: FusionEngine.analyze() detailed")
print("=" * 60)

# Create producer: sin(x) for shape [10, 20]
x_mem = Memory.defglobal((10, 20), float32)
x_load = Load.from_tensor(x_mem)
x_sin = Sin((x_load,))
x_out = Memory.defglobal((10, 20), float32, tmp=True)
x_store = Store.new(Load.from_tensor(x_out), x_sin)
producer = Sync.sync(x_out, x_store)

# Create consumer: sin of producer output for same shape
y_load = Load.from_tensor(producer)
y_sin = Sin((y_load,))
y_out = Memory.defglobal((10, 20), float32, tmp=True)
y_store = Store.new(Load.from_tensor(y_out), y_sin)
consumer = Sync.sync(y_out, y_store)

# Analyze fusion
fusion_result = FusionEngine.analyze(consumer, producer)
print("Fusion analysis result:")
print(f"  fusible: {fusion_result.fusible}")
print(f"  fusion_type: {fusion_result.fusion_type}")
print(f"  reason: {fusion_result.reason}")
if fusion_result.morphism:
    print(f"  morphism dims: {list(fusion_result.morphism.keys())}")

print()
print("=" * 60)
print("Test 8: Matmul + Softmax + Matmul (Attention Pattern)")
print("=" * 60)

# Attention: Softmax(Q @ K^T) @ V
# Q: [batch, seq_len, d_k]
# K: [batch, seq_len, d_k]
# V: [batch, seq_len, d_v]
# Output: [batch, seq_len, d_v]

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
scores = Q_2d @ K_2d  # [seq_len, seq_len]

# Softmax on last axis
attn_weights = scores.softmax(axis=-1)  # [seq_len, seq_len]

# Output: attention @ V
output = attn_weights @ V_2d  # [seq_len, d_v]

print(f"Q shape: {[ax.size for ax in Q_2d.op.T.axes]}")
print(f"K^T shape: {[ax.size for ax in K_2d.op.T.axes]}")
print(f"V shape: {[ax.size for ax in V_2d.op.T.axes]}")
print(f"Scores shape (Q@K^T): {[ax.size for ax in scores.op.T.axes]}")
print(f"Attn weights shape: {[ax.size for ax in attn_weights.op.T.axes]}")
print(f"Output shape: {[ax.size for ax in output.op.T.axes]}")

# Lower and check
lowered_attn = output.op.lower()
print(f"\nLowered attention type: {type(lowered_attn).__name__}")
if isinstance(lowered_attn, Sync):
    print(f"Sync dims: {lowered_attn.dims}")
    sources = lowered_attn.load_sources()
    print(f"Kernel boundaries: {len(sources)}")
    # Print IR graph
    print("\n--- IR Graph ---")
    print(lowered_attn.viz())

print()
print("=" * 60)
print("All tests completed!")
print("=" * 60)
