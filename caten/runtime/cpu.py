from typing import Any

import caten as C
import caten.ir as ir


class CPUTensor(C.TensorImpl):
    def allocate(self) -> None:
        pass

    def free(self) -> None:
        pass

    def compile(self) -> None:
        pass

    @staticmethod
    def render(op: Any) -> str:
        """
        Render IR to C-like pseudocode using simple recursion.
        
        Strategy:
        1. Collect all Exec nodes in topological order (dependencies first)
        2. Render each Exec as a separate kernel
        3. MemoryOf just returns the buffer name (Exec already rendered)
        """
        def emit_expr(node: Any) -> str:
            """Emit an expression (no side effects, returns string)."""
            nid = id(node)
            match node:
                case ir.Const():
                    if isinstance(node.value, float):
                        return f"{node.value}f"
                    return str(node.value)

                case ir.Dim():
                    return emit_expr(node.range)

                case ir.Range():
                    return f"{node.name}"

                case ir.Add():
                    a, b = emit_expr(node.args[0]), emit_expr(node.args[1])
                    return f"({a}+{b})"

                case ir.Mul():
                    a, b = emit_expr(node.args[0]), emit_expr(node.args[1])
                    return f"({a}*{b})"

                case ir.IDiv():
                    a, b = emit_expr(node.args[0]), emit_expr(node.args[1])
                    return f"(({a})/({b}))"

                case ir.Mod():
                    a, b = emit_expr(node.args[0]), emit_expr(node.args[1])
                    return f"(({a})%({b}))"

                case ir.Neg():
                    a = emit_expr(node.args[0])
                    return f"(-{a})"

                case ir.Memory():
                    if nid in var_map:
                        return var_map[nid]
                    v = fresh("buf")
                    var_map[nid] = v
                    if node.T[0] and node.T[0].axes:
                        shape = " * ".join(get_size(ax.size) for ax in node.T[0].axes)
                    else:
                        shape = "1"
                    dtype = node.T[0].dtype.name if node.T[0] else "float"
                    c_type = "float" if "float" in dtype else "int"
                    decls.append(f"{c_type} {v}[{shape}];")
                    return v

                case ir.MemoryOf():
                    # Exec should already be rendered; just return buffer name
                    exec_node = node.args[0]
                    if id(exec_node) in var_map:
                        var_map[nid] = var_map[id(exec_node)]
                        return var_map[nid]
                    # Fallback: render the exec (shouldn't happen with proper ordering)
                    render_exec(exec_node)
                    if id(exec_node) in var_map:
                        var_map[nid] = var_map[id(exec_node)]
                        return var_map[nid]
                    return "/* MemoryOf error */"

                case ir.Load():
                    buf = emit_expr(node.args[0])
                    if len(node.args) > 1:
                        idx = emit_expr(node.args[1])
                    else:
                        idx = "0"
                    return f"{buf}[{idx}]"

                case _:
                    return f"/* expr: {node.__class__.__name__} */"
        return emit_expr(op)


C.ATen.register("CPU", CPUTensor)
