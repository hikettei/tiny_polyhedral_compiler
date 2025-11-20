import ctypes
from ctypes import c_char_p, c_void_p

# libisl.dylib をロードする
# Homebrew なら /opt/homebrew/opt/isl/lib/libisl.**.dylib みたいなパスになっているはず
libisl = ctypes.CDLL("libisl.dylib")

# オペークポインタ型
isl_ctx_p = c_void_p
isl_set_p = c_void_p

# ctx の生成と解放
libisl.isl_ctx_alloc.argtypes = []
libisl.isl_ctx_alloc.restype = isl_ctx_p

libisl.isl_ctx_free.argtypes = [isl_ctx_p]
libisl.isl_ctx_free.restype = None

# isl_set_read_from_str のシグネチャ
libisl.isl_set_read_from_str.argtypes = [isl_ctx_p, c_char_p]
libisl.isl_set_read_from_str.restype = isl_set_p

# isl_set_free も定義しておく
libisl.isl_set_free.argtypes = [isl_set_p]
libisl.isl_set_free.restype = None

# --- 実際の呼び出し ---

# コンテキストを作る
ctx = libisl.isl_ctx_alloc()
if not ctx:
    raise RuntimeError("isl_ctx_alloc failed")

# Python の str を C の char*（UTF-8）に変換
s = "{ [i] : 0 <= i < 10 }"
s_bytes = s.encode("utf-8")  # str -> bytes

print(ctx)
print(s_bytes)
# bytes は c_char_p に自動変換されるので、そのまま渡してOK
iset = libisl.isl_set_read_from_str(ctx, s_bytes)
if not iset:
    # エラー時は NULL が返るのでチェックしておく
    libisl.isl_ctx_free(ctx)
    raise RuntimeError("isl_set_read_from_str failed")

# ここで iset を他の isl_* 関数に渡して色々操作する

# 後始末
libisl.isl_set_free(iset)
libisl.isl_ctx_free(ctx)

