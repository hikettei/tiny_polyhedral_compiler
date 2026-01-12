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
        decls: list[str] = []
        kernels: list[str] = []  # Each kernel as a string block
        var_map: dict[int, str] = {}  # node id -> variable name
        rendered_exec: set[int] = set()
        counter = [0]
        kernel_counter = [0]

        def fresh(prefix: str = "v") -> str:
            name = f"{prefix}{counter[0]}"
            counter[0] += 1
            return name

        def collect_execs(node: Any, visited: set[int], result: list) -> None:
            """Collect all Exec nodes in topological order (post-order DFS)."""
            nid = id(node)
            if nid in visited:
                return
            visited.add(nid)
            
            if hasattr(node, 'args'):
                for arg in node.args:
                    collect_execs(arg, visited, result)
            
            if isinstance(node, ir.Exec):
                result.append(node)

        def get_size(node: ir.ATenOp) -> str:
            """Get size as string."""
            if isinstance(node, ir.Const):
                return str(node.value)
            return emit_expr(node)

        def emit_expr(node: Any) -> str:
            """Emit an expression (no side effects, returns string)."""
            nid = id(node)
            if nid in var_map:
                return var_map[nid]

            match node:
                case ir.Const():
                    if isinstance(node.value, float):
                        return f"{node.value}f"
                    return str(node.value)

                case ir.Dim():
                    # Use kernel-local loop variable
                    return f"i{node.dim}"

                case ir.Aff():
                    stride = emit_expr(node.args[0])
                    dim_expr = emit_expr(node.args[1])
                    offset = emit_expr(node.args[2])
                    incf = emit_expr(node.args[3])
                    
                    if incf == "1" and offset == "0":
                        if stride == "1":
                            return dim_expr
                        return f"({stride} * {dim_expr})"
                    if incf == "1":
                        if stride == "1":
                            return f"({dim_expr} + {offset})"
                        return f"({stride} * ({dim_expr} + {offset}))"
                    if offset == "0":
                        if stride == "1":
                            return f"({incf} * {dim_expr})"
                        return f"({stride} * {incf} * {dim_expr})"
                    if stride == "1":
                        return f"({incf} * {dim_expr} + {offset})"
                    return f"({stride} * ({incf} * {dim_expr} + {offset}))"

                case ir.AccessMap():
                    affs = node.affs
                    if not affs:
                        return "0"
                    parts = [emit_expr(aff) for aff in affs]
                    parts = [p for p in parts if p != "0"]
                    if not parts:
                        return "0"
                    return " + ".join(parts)

                case ir.Add():
                    a, b = emit_expr(node.args[0]), emit_expr(node.args[1])
                    return f"({a} + {b})"

                case ir.Mul():
                    a, b = emit_expr(node.args[0]), emit_expr(node.args[1])
                    return f"({a} * {b})"

                case ir.IDiv():
                    a, b = emit_expr(node.args[0]), emit_expr(node.args[1])
                    return f"(({a}) / ({b}))"

                case ir.Mod():
                    a, b = emit_expr(node.args[0]), emit_expr(node.args[1])
                    return f"(({a}) % ({b}))"

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

        def emit_stmt(node: Any, lines: list[str], indent: int) -> str:
            """Emit a statement (may have side effects). Returns result expression."""
            ind = "  " * indent
            nid = id(node)
            
            if nid in var_map:
                return var_map[nid]

            match node:
                case ir.Store():
                    dst = emit_stmt(node.args[0], lines, indent)
                    src = emit_stmt(node.args[1], lines, indent)
                    lines.append(f"{ind}{dst} = {src};")
                    return dst

                case ir.Load():
                    buf = emit_expr(node.args[0])
                    if len(node.args) > 1:
                        idx = emit_expr(node.args[1])
                    else:
                        idx = "0"
                    return f"{buf}[{idx}]"

                case ir.Sin():
                    a = emit_stmt(node.args[0], lines, indent)
                    v = fresh()
                    var_map[nid] = v
                    lines.append(f"{ind}float {v} = sinf({a});")
                    return v

                case ir.Exp2():
                    a = emit_stmt(node.args[0], lines, indent)
                    v = fresh()
                    var_map[nid] = v
                    lines.append(f"{ind}float {v} = exp2f({a});")
                    return v

                case ir.Log2():
                    a = emit_stmt(node.args[0], lines, indent)
                    v = fresh()
                    var_map[nid] = v
                    lines.append(f"{ind}float {v} = log2f({a});")
                    return v

                case ir.Sqrt():
                    a = emit_stmt(node.args[0], lines, indent)
                    v = fresh()
                    var_map[nid] = v
                    lines.append(f"{ind}float {v} = sqrtf({a});")
                    return v

                case ir.Recip():
                    a = emit_stmt(node.args[0], lines, indent)
                    v = fresh()
                    var_map[nid] = v
                    lines.append(f"{ind}float {v} = 1.0f / {a};")
                    return v

                case ir.Neg():
                    a = emit_stmt(node.args[0], lines, indent)
                    v = fresh()
                    var_map[nid] = v
                    lines.append(f"{ind}float {v} = -{a};")
                    return v

                case ir.Add():
                    a = emit_stmt(node.args[0], lines, indent)
                    b = emit_stmt(node.args[1], lines, indent)
                    v = fresh()
                    var_map[nid] = v
                    lines.append(f"{ind}float {v} = {a} + {b};")
                    return v

                case ir.Mul():
                    a = emit_stmt(node.args[0], lines, indent)
                    b = emit_stmt(node.args[1], lines, indent)
                    v = fresh()
                    var_map[nid] = v
                    lines.append(f"{ind}float {v} = {a} * {b};")
                    return v

                case ir.Max():
                    a = emit_stmt(node.args[0], lines, indent)
                    b = emit_stmt(node.args[1], lines, indent)
                    v = fresh()
                    var_map[nid] = v
                    lines.append(f"{ind}float {v} = ({a} > {b}) ? {a} : {b};")
                    return v

                case ir.Where():
                    cond = emit_stmt(node.args[0], lines, indent)
                    a = emit_stmt(node.args[1], lines, indent)
                    b = emit_stmt(node.args[2], lines, indent)
                    v = fresh()
                    var_map[nid] = v
                    lines.append(f"{ind}float {v} = {cond} ? {a} : {b};")
                    return v

                case ir.Lt():
                    a = emit_stmt(node.args[0], lines, indent)
                    b = emit_stmt(node.args[1], lines, indent)
                    return f"({a} < {b})"

                case ir.Neq():
                    a = emit_stmt(node.args[0], lines, indent)
                    b = emit_stmt(node.args[1], lines, indent)
                    return f"({a} != {b})"

                case _:
                    # Fallback to expression
                    return emit_expr(node)

        def render_exec(exec_node: ir.Exec) -> None:
            """Render an Exec as a kernel."""
            nid = id(exec_node)
            if nid in rendered_exec:
                return
            rendered_exec.add(nid)

            lines: list[str] = []
            kernel_id = kernel_counter[0]
            kernel_counter[0] += 1

            # Get output and body
            output = exec_node.args[exec_node.n_dims]
            body = exec_node.body
            dims = exec_node.dim_nodes

            # Find the actual output buffer (unwrap MemoryOf/Load chains)
            def get_base_memory(node: Any) -> Any:
                if isinstance(node, ir.Memory):
                    return node
                if isinstance(node, ir.MemoryOf):
                    return get_base_memory(node.args[0])
                if isinstance(node, ir.Load):
                    return get_base_memory(node.args[0])
                if isinstance(node, ir.Exec):
                    # For Exec, find its output memory
                    return get_base_memory(node.args[node.n_dims])
                return node

            base_output = get_base_memory(output)
            
            # Emit output buffer (use base memory for var_map)
            out_var = emit_expr(base_output)
            var_map[nid] = out_var

            lines.append(f"// Kernel {kernel_id}: output -> {out_var}")
            # Emit loop nest
            indent = 0
            loop_dims: list[tuple[ir.Dim, str, str]] = []
            for dim in dims:
                rng = dim.range
                size_node = rng.args[0]
                size = get_size(size_node)
                
                # Skip size-1 loops
                if isinstance(size_node, ir.Const) and size_node.value == 1:
                    continue
                
                loop_var = f"i{dim.dim}"
                loop_dims.append((dim, loop_var, size))
                lines.append(f"{'  ' * indent}for (int {loop_var} = 0; {loop_var} < {size}; {loop_var}++) {{")
                indent += 1

            # Emit body
            emit_stmt(body, lines, indent)

            # Close loops
            for _ in loop_dims:
                indent -= 1
                lines.append(f"{'  ' * indent}}}")

            kernels.append("\n".join(lines))

        # Step 1: Collect all Exec nodes in topological order
        all_execs: list[ir.Exec] = []
        collect_execs(op, set(), all_execs)

        # Step 2: Render each Exec as a kernel
        for exec_node in all_execs:
            render_exec(exec_node)

        # Assemble output
        result = []
        if decls:
            result.append("// Buffers")
            result.extend(decls)
            result.append("")
        if kernels:
            result.extend(kernels)
        return "\n".join(result)


C.ATen.register("CPU", CPUTensor)
