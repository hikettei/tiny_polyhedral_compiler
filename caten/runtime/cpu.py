from typing import Any, Dict, List, Set

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
        Render IR to C code following "1 op = 1 line" rule.

        Each IR node produces exactly one line of C code (or loop structure).
        """
        # TODO: This is ugly, reimpl!
        declarations: List[str] = []
        body_lines: List[str] = []
        var_counter = [0]
        node_to_var: Dict[int, str] = {}
        rendered_endranges: Set[int] = set()
        indent_level = [0]

        def indent() -> str:
            return "  " * indent_level[0]

        def new_var(prefix: str = "v") -> str:
            name = f"{prefix}{var_counter[0]}"
            var_counter[0] += 1
            return name

        def _render(node: Any, lines: List[str]) -> str:
            nid = id(node)
            if nid in node_to_var:
                return node_to_var[nid]

            match node:
                # Constants - inline
                case ir.Const():
                    if isinstance(node.value, float):
                        return f"{node.value}f"
                    return f"{node.value}"

                # Range - loop variable
                case ir.Range():
                    return f"i{node.dim}"

                # Affine index: stride * (incf * range + offset)
                case ir.Aff():
                    stride = _render(node.args[0], lines)
                    range_var = _render(node.args[1], lines)
                    offset = _render(node.args[2], lines)
                    incf = _render(node.args[3], lines)
                    if incf == "1" and offset == "0":
                        return f"({stride} * {range_var})"
                    elif incf == "1":
                        return f"({stride} * ({range_var} + {offset}))"
                    elif offset == "0":
                        return f"({stride} * ({incf} * {range_var}))"
                    else:
                        return f"({stride} * ({incf} * {range_var} + {offset}))"

                # Memory - buffer name
                case ir.Memory():
                    var = new_var("buf")
                    node_to_var[nid] = var
                    # Emit declaration at top
                    if node.T.axes:
                        shape_str = " * ".join(str(s.size.value) for s in node.T.axes)
                    else:
                        shape_str = "1"
                    declarations.append(f"float {var}[{shape_str}];")
                    return var

                # Load - array access
                case ir.Load():
                    buf = _render(node.args[0], lines)
                    indices = [_render(arg, lines) for arg in node.args[1:]]
                    idx_expr = " + ".join(indices) if indices else "0"
                    return f"{buf}[{idx_expr}]"

                # Store - assignment (returns dst for chaining)
                case ir.Store():
                    dst = _render(node.args[0], lines)
                    src = _render(node.args[1], lines)
                    lines.append(f"{indent()}{dst} = {src};")
                    return dst

                # Binary ops - 1 line each
                case ir.Add():
                    a = _render(node.args[0], lines)
                    b = _render(node.args[1], lines)
                    var = new_var()
                    node_to_var[nid] = var
                    lines.append(f"{indent()}float {var} = {a} + {b};")
                    return var

                case ir.Mul():
                    a = _render(node.args[0], lines)
                    b = _render(node.args[1], lines)
                    var = new_var()
                    node_to_var[nid] = var
                    lines.append(f"{indent()}float {var} = {a} * {b};")
                    return var

                case ir.Max():
                    a = _render(node.args[0], lines)
                    b = _render(node.args[1], lines)
                    var = new_var()
                    node_to_var[nid] = var
                    lines.append(f"{indent()}float {var} = ({a} > {b}) ? {a} : {b};")
                    return var

                # Unary ops
                case ir.Neg():
                    a = _render(node.args[0], lines)
                    var = new_var()
                    node_to_var[nid] = var
                    lines.append(f"{indent()}float {var} = -{a};")
                    return var

                case ir.Recip():
                    a = _render(node.args[0], lines)
                    var = new_var()
                    node_to_var[nid] = var
                    lines.append(f"{indent()}float {var} = 1.0f / {a};")
                    return var

                case ir.Sin():
                    a = _render(node.args[0], lines)
                    var = new_var()
                    node_to_var[nid] = var
                    lines.append(f"{indent()}float {var} = sinf({a});")
                    return var

                case ir.Exp2():
                    a = _render(node.args[0], lines)
                    var = new_var()
                    node_to_var[nid] = var
                    lines.append(f"{indent()}float {var} = exp2f({a});")
                    return var

                case ir.Log2():
                    a = _render(node.args[0], lines)
                    var = new_var()
                    node_to_var[nid] = var
                    lines.append(f"{indent()}float {var} = log2f({a});")
                    return var

                case ir.Sqrt():
                    a = _render(node.args[0], lines)
                    var = new_var()
                    node_to_var[nid] = var
                    lines.append(f"{indent()}float {var} = sqrtf({a});")
                    return var

                # EndRange - loop structure
                case ir.EndRange():
                    # If already rendered, just return the buffer var
                    if nid in rendered_endranges:
                        return node_to_var.get(nid, "/* err */")
                    rendered_endranges.add(nid)

                    # Get memory and store
                    memory = node.args[0]
                    store = node.args[1]

                    # First, find and render any nested EndRanges
                    def find_inner_endranges(n: Any) -> List[ir.EndRange]:
                        result = []
                        if isinstance(n, ir.EndRange) and id(n) != nid:
                            result.append(n)
                        elif hasattr(n, 'args'):
                            for arg in n.args:
                                result.extend(find_inner_endranges(arg))
                        return result

                    inner_endranges = find_inner_endranges(store)
                    for inner in inner_endranges:
                        _render(inner, lines)

                    # Collect Range info from the store (excluding inner EndRanges)
                    ranges: Dict[int, ir.Range] = {}
                    def collect_ranges(n: Any) -> None:
                        if isinstance(n, ir.Range):
                            ranges[n.dim] = n
                        if isinstance(n, ir.EndRange) and id(n) != nid:
                            return  # Don't recurse into other EndRanges
                        if hasattr(n, 'args'):
                            for arg in n.args:
                                collect_ranges(arg)
                    collect_ranges(store)

                    # Generate output buffer first
                    out_var = _render(memory, lines)
                    node_to_var[nid] = out_var

                    # Generate loop headers
                    for dim in sorted(ranges.keys()):
                        rng = ranges[dim]
                        size = _render(rng.args[0], lines)
                        loop_var = f"i{dim}"
                        is_reduce = dim in node.reduce_dims
                        comment = " // reduce" if is_reduce else ""
                        lines.append(f"{indent()}for (int {loop_var} = 0; {loop_var} < {size}; {loop_var}++) {{{comment}")
                        indent_level[0] += 1

                    # Render the store body
                    _render(store, lines)

                    # Close loops
                    for _ in ranges:
                        indent_level[0] -= 1
                        lines.append(f"{indent()}}}")

                    return out_var

                case _:
                    return f"/* unsupported: {node.__class__.__name__} */"

        _render(op, body_lines)

        # Combine declarations and body
        result = []
        if declarations:
            result.append("// Buffers")
            result.extend(declarations)
            result.append("")
        result.append("// Computation")
        result.extend(body_lines)

        return "\n".join(result)


C.ATen.register("CPU", CPUTensor)
