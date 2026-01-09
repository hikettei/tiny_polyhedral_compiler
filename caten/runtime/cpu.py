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
        Render IR to C code. Clean implementation using new EndRange structure.

        EndRange structure: args = (ranges..., output, body)
        - .ranges: Range nodes defining iteration space
        - .output: Memory node for output array
        - .body: Store node with computation (or nested EndRange for reduction)
        """
        decls: list[str] = []
        lines: list[str] = []
        var_map: dict[int, str] = {}
        rendered: set[int] = set()
        indent = 0
        counter = [0]

        def ind() -> str:
            return "  " * indent

        def var(prefix: str = "v") -> str:
            name = f"{prefix}{counter[0]}"
            counter[0] += 1
            return name

        def emit(node: Any) -> str:
            nonlocal indent
            nid = id(node)
            if nid in var_map:
                return var_map[nid]

            match node:
                case ir.Const():
                    return f"{node.value}f" if isinstance(node.value, float) else str(node.value)

                case ir.Range():
                    # Check if this range was mapped (e.g., size-1 ranges map to "0")
                    if nid in var_map:
                        return var_map[nid]
                    return f"i{node.dim}"

                case ir.Aff():
                    s, r, o, i = emit(node.args[0]), emit(node.args[1]), emit(node.args[2]), emit(node.args[3])
                    if i == "1" and o == "0": return f"({s} * {r})"
                    if i == "1": return f"({s} * ({r} + {o}))"
                    if o == "0": return f"({s} * ({i} * {r}))"
                    return f"({s} * ({i} * {r} + {o}))"

                case ir.Memory():
                    v = var("buf")
                    var_map[nid] = v
                    shape = " * ".join(str(a.size.value) for a in node.T.axes) if node.T.axes else "1"
                    decls.append(f"float {v}[{shape}];")
                    return v

                case ir.Load():
                    buf = emit(node.args[0])
                    idx = " + ".join(emit(a) for a in node.args[1:]) or "0"
                    return f"{buf}[{idx}]"

                case ir.Store():
                    dst, src = emit(node.args[0]), emit(node.args[1])
                    lines.append(f"{ind()}{dst} = {src};")
                    return dst

                case ir.Add():
                    a, b = emit(node.args[0]), emit(node.args[1])
                    v = var()
                    var_map[nid] = v
                    lines.append(f"{ind()}float {v} = {a} + {b};")
                    return v

                case ir.Mul():
                    a, b = emit(node.args[0]), emit(node.args[1])
                    v = var()
                    var_map[nid] = v
                    lines.append(f"{ind()}float {v} = {a} * {b};")
                    return v

                case ir.Max():
                    a, b = emit(node.args[0]), emit(node.args[1])
                    v = var()
                    var_map[nid] = v
                    lines.append(f"{ind()}float {v} = ({a} > {b}) ? {a} : {b};")
                    return v

                case ir.Neg():
                    a = emit(node.args[0])
                    v = var()
                    var_map[nid] = v
                    lines.append(f"{ind()}float {v} = -{a};")
                    return v

                case ir.Recip():
                    a = emit(node.args[0])
                    v = var()
                    var_map[nid] = v
                    lines.append(f"{ind()}float {v} = 1.0f / {a};")
                    return v

                case ir.Sin():
                    a = emit(node.args[0])
                    v = var()
                    var_map[nid] = v
                    lines.append(f"{ind()}float {v} = sinf({a});")
                    return v

                case ir.Exp2():
                    a = emit(node.args[0])
                    v = var()
                    var_map[nid] = v
                    lines.append(f"{ind()}float {v} = exp2f({a});")
                    return v

                case ir.Log2():
                    a = emit(node.args[0])
                    v = var()
                    var_map[nid] = v
                    lines.append(f"{ind()}float {v} = log2f({a});")
                    return v

                case ir.Sqrt():
                    a = emit(node.args[0])
                    v = var()
                    var_map[nid] = v
                    lines.append(f"{ind()}float {v} = sqrtf({a});")
                    return v

                case ir.EndRange():
                    if nid in rendered:
                        return var_map.get(nid, "/* err */")
                    rendered.add(nid)

                    # Render data source EndRanges as separate kernels first
                    for src in node.load_sources():
                        emit(src)

                    # Emit output buffer
                    out_var = emit(node.output)
                    var_map[nid] = out_var

                    # Emit loop nest (skip size-1 dimensions)
                    loop_ranges = []
                    for rng in node.ranges:
                        if isinstance(rng, ir.Range):
                            size_val = rng.args[0]
                            # Skip size-1 loops - map their index to 0
                            if isinstance(size_val, ir.Const) and size_val.value == 1:
                                var_map[id(rng)] = "0"
                                continue
                            loop_ranges.append(rng)
                            size = emit(size_val)
                            lines.append(f"{ind()}for (int i{rng.dim} = 0; i{rng.dim} < {size}; i{rng.dim}++) {{")
                            indent += 1

                    # Emit body (inline EndRanges like reductions are rendered here)
                    emit(node.body)

                    # Close loops
                    for _ in loop_ranges:
                        indent -= 1
                        lines.append(f"{ind()}}}")

                    return out_var

                case _:
                    return f"/* unsupported: {node.__class__.__name__} */"

        emit(op)

        result = []
        if decls:
            result.append("// Buffers")
            result.extend(decls)
            result.append("")
        result.append("// Computation")
        result.extend(lines)
        return "\n".join(result)


C.ATen.register("CPU", CPUTensor)
