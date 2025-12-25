# caten/viz.py
from __future__ import annotations

from typing import Any, Dict, List, Optional, Set, Tuple

import caten.ir as ir
from caten.ir import ATenOp


# ===== ANSI styling ==========================================================
class _Ansi:
    RESET = "\x1b[0m"
    BOLD = "\x1b[1m"
    DIM = "\x1b[2m"

    FG_GRAY = "\x1b[90m"
    FG_RED = "\x1b[31m"
    FG_GREEN = "\x1b[32m"
    FG_YELLOW = "\x1b[33m"
    FG_BLUE = "\x1b[34m"
    FG_MAGENTA = "\x1b[35m"
    FG_CYAN = "\x1b[36m"


def _style(s: str, *, fg: Optional[str] = None, bold: bool = False, dim: bool = False, color: bool = True) -> str:
    if not color:
        return s
    pre = ""
    if bold:
        pre += _Ansi.BOLD
    if dim:
        pre += _Ansi.DIM
    if fg:
        pre += fg
    if not pre:
        return s
    return f"{pre}{s}{_Ansi.RESET}"


# ===== Small expression pretty-printer (for shape/stride/offset) =============
def _is_scalar_const(x: ATenOp) -> bool:
    return isinstance(x, ir.Const) and isinstance(x.value, (int, float, bool, str))


def expr_to_str(x: Any, *, max_depth: int = 3, max_len: int = 60) -> str:
    """
    Make a compact string for scalar-ish ATenOp expressions.
    Intended for shape, stride, offset rendering.
    """
    def go(n: Any, depth: int) -> str:
        if not isinstance(n, ATenOp):
            return repr(n)

        # Constant folded scalar
        if isinstance(n, ir.Const):
            v = n.value
            if isinstance(v, bool):
                return "true" if v else "false"
            if isinstance(v, (int, float)):
                return str(v)
            return repr(v)

        # If it is scalar and constant-folded via .item
        try:
            it = n.item  # type: ignore[attr-defined]
            if isinstance(it, (int, float)):
                return str(it)
        except Exception:
            pass

        if depth >= max_depth:
            return f"{type(n).__name__}"

        # Common ops as infix
        if isinstance(n, ir.Add):
            a, b = n.args
            return f"({go(a, depth+1)}+{go(b, depth+1)})"
        if isinstance(n, ir.Mul):
            a, b = n.args
            return f"({go(a, depth+1)}*{go(b, depth+1)})"
        if isinstance(n, ir.IDiv):
            a, b = n.args
            return f"({go(a, depth+1)}//{go(b, depth+1)})"
        if isinstance(n, ir.Mod):
            a, b = n.args
            return f"({go(a, depth+1)}%{go(b, depth+1)})"
        if isinstance(n, ir.Neg):
            (a,) = n.args
            return f"(-{go(a, depth+1)})"
        if isinstance(n, ir.Range):
            (sz,) = n.args
            return f"range({go(sz, depth+1)})"
        if isinstance(n, ir.Aff):
            # Aff(stride, range, offset, incx)
            st, rg, off, inc = n.args
            return f"aff(st={go(st, depth+1)},rg={go(rg, depth+1)},off={go(off, depth+1)},inc={go(inc, depth+1)})"

        # Fallback
        return f"{type(n).__name__}"

    out = go(x, 0)
    if len(out) > max_len:
        out = out[: max(0, max_len - 1)] + "…"
    return out


# ===== Graph traversal =======================================================
def _children(n: ATenOp, *, include_predecessors: bool = False) -> Tuple[ATenOp, ...]:
    if include_predecessors:
        # Might explode the graph because it includes shape/stride expressions.
        try:
            return tuple(n.predecessors)  # type: ignore[attr-defined]
        except Exception:
            return tuple(n.args)
    return tuple(n.args)


def _assign_ids(root: ATenOp, *, include_predecessors: bool) -> Dict[int, int]:
    """
    Deterministic-ish numbering by DFS order.
    """
    ids: Dict[int, int] = {}
    stack: List[ATenOp] = [root]
    while stack:
        n = stack.pop()
        k = id(n)
        if k in ids:
            continue
        ids[k] = len(ids)
        # push children reversed to keep left-to-right stable
        ch = _children(n, include_predecessors=include_predecessors)
        for c in reversed(ch):
            stack.append(c)
    return ids


# ===== Node label ============================================================
def _dtype_str(n: ATenOp) -> str:
    if n.T is None:
        return "<?>"
    return n.T.dtype.name


def _shape_str(n: ATenOp) -> str:
    if n.T is None:
        return "<?>"
    if n.T.ndim == 0:
        return "[]"
    dims = [expr_to_str(ax.size) for ax in n.T.axes]
    return "[" + ",".join(dims) + "]"


def _strides_str(n: ATenOp) -> str:
    if n.T is None:
        return "<?>"
    if n.T.ndim == 0:
        return "[]"
    st = [expr_to_str(ax.stride) for ax in n.T.axes]
    return "[" + ",".join(st) + "]"


def _node_kind_color(n: ATenOp) -> str:
    if isinstance(n, ir.Const):
        return _Ansi.FG_YELLOW
    if isinstance(n, ir.Allocate):
        return _Ansi.FG_GREEN
    if isinstance(n, (ir.View, ir.Reduce)):
        return _Ansi.FG_CYAN
    if isinstance(n, (ir.Aref, ir.Aff, ir.Range, ir.Index)):
        return _Ansi.FG_MAGENTA
    # arithmetic default
    return _Ansi.FG_BLUE


def _node_line(
    n: ATenOp,
    nid: int,
    *,
    show_shape: bool,
    show_strides: bool,
    show_offset: bool,
    show_value: bool,
    color: bool,
) -> str:
    op_name = type(n).__name__

    # Value for constants
    extra = ""
    if show_value and isinstance(n, ir.Const):
        extra = f" val={expr_to_str(n)}"

    # Reduce op type
    if isinstance(n, ir.Reduce):
        try:
            extra = f" op={n.op.__name__}"  # type: ignore[attr-defined]
        except Exception:
            pass

    dtype = _dtype_str(n)
    shape = _shape_str(n) if show_shape else ""
    strides = _strides_str(n) if (show_shape and show_strides) else ""
    offs = ""
    if show_shape and show_offset and n.T is not None and n.T.offset is not None:
        offs = expr_to_str(n.T.offset)

    head = f"({nid:03d}) {op_name}"
    head = _style(head, fg=_node_kind_color(n), bold=True, color=color)

    meta_bits: List[str] = []
    if n.T is not None:
        meta_bits.append(_style(dtype, fg=_Ansi.FG_RED, color=color))
        if show_shape:
            meta_bits.append(_style(shape, fg=_Ansi.FG_GRAY, color=color))
        if show_shape and show_strides:
            meta_bits.append(_style(f"st={strides}", fg=_Ansi.FG_GRAY, dim=True, color=color))
        if show_shape and show_offset and offs:
            meta_bits.append(_style(f"off={offs}", fg=_Ansi.FG_GRAY, dim=True, color=color))
    else:
        meta_bits.append(_style("T=None", fg=_Ansi.FG_GRAY, dim=True, color=color))

    meta = " ".join(meta_bits)
    if extra:
        meta += _style(extra, fg=_Ansi.FG_GRAY, dim=True, color=color)
    return f"{head} {meta}".rstrip()


# ===== ASCII tree renderer (DAG-aware) ======================================
def render(
    root: ATenOp,
    *,
    include_predecessors: bool = False,
    max_depth: int = 200,
    max_nodes: int = 2000,
    show_shape: bool = True,
    show_strides: bool = False,
    show_offset: bool = False,
    show_value: bool = True,
    color: bool = True,
    unicode: bool = True,
) -> str:
    """
    Pretty print ATenOp graph as a tree-ish ASCII art.
    For DAG sharing, repeated nodes are printed as a reference line.
    """

    ids = _assign_ids(root, include_predecessors=include_predecessors)
    visited_expanded: Set[int] = set()
    visiting: Set[int] = set()
    printed = 0

    if unicode:
        TEE = "├─"
        ELBOW = "└─"
        PIPE = "│ "
        SPACE = "  "
        REF = "↩"
    else:
        TEE = "|-"
        ELBOW = "`-"
        PIPE = "| "
        SPACE = "  "
        REF = "<-"

    def line_prefix(prefix_parts: List[bool]) -> str:
        # True means there are more siblings at that level.
        out = ""
        for has_more in prefix_parts:
            out += PIPE if has_more else SPACE
        return out

    def go(n: ATenOp, prefix_parts: List[bool], is_last: bool, depth: int) -> List[str]:
        nonlocal printed
        if printed >= max_nodes:
            return [line_prefix(prefix_parts) + _style("… truncated (max_nodes)", fg=_Ansi.FG_GRAY, dim=True, color=color)]
        if depth > max_depth:
            return [line_prefix(prefix_parts) + _style("… truncated (max_depth)", fg=_Ansi.FG_GRAY, dim=True, color=color)]

        k = id(n)
        nid = ids.get(k, -1)

        # cycle protection
        if k in visiting:
            return [line_prefix(prefix_parts) + _style(f"{REF} ({nid:03d}) cycle", fg=_Ansi.FG_RED, bold=True, color=color)]

        branch = ""
        if prefix_parts:
            branch = (ELBOW if is_last else TEE) + " "
        head = _node_line(
            n,
            nid,
            show_shape=show_shape,
            show_strides=show_strides,
            show_offset=show_offset,
            show_value=show_value,
            color=color,
        )

        # If already expanded once, show only reference
        if k in visited_expanded:
            ref_line = line_prefix(prefix_parts) + branch + _style(f"{REF} ", fg=_Ansi.FG_GRAY, dim=True, color=color) + head
            printed += 1
            return [ref_line]

        # Print the node
        out_lines = [line_prefix(prefix_parts) + branch + head]
        printed += 1

        # Expand children
        ch = _children(n, include_predecessors=include_predecessors)
        if not ch:
            visited_expanded.add(k)
            return out_lines

        visiting.add(k)
        try:
            visited_expanded.add(k)
            # Next level prefix: whether current level has more siblings
            next_prefix = prefix_parts + [not is_last] if prefix_parts else []
            # When root, prefix_parts is empty, but we still want children indentation
            if not prefix_parts:
                next_prefix = [False]  # one indentation level

            for i, c in enumerate(ch):
                last = (i == len(ch) - 1)
                out_lines.extend(go(c, next_prefix, last, depth + 1))
        finally:
            visiting.remove(k)

        return out_lines

    lines = []
    # Root printed without branch
    root_id = ids.get(id(root), 0)
    lines.append(
        _node_line(
            root,
            root_id,
            show_shape=show_shape,
            show_strides=show_strides,
            show_offset=show_offset,
            show_value=show_value,
            color=color,
        )
    )
    visited_expanded.add(id(root))
    printed += 1

    ch0 = _children(root, include_predecessors=include_predecessors)
    for i, c in enumerate(ch0):
        lines.extend(go(c, [False], i == len(ch0) - 1, 1))

    return "\n".join(lines)


# ===== DOT export (bonus) ====================================================
def to_dot(root: ATenOp, *, include_predecessors: bool = False, show_shape: bool = True) -> str:
    """
    Generate Graphviz DOT as a fallback when ASCII is not enough.
    """
    ids = _assign_ids(root, include_predecessors=include_predecessors)
    nodes: List[str] = []
    edges: List[str] = []

    seen: Set[int] = set()
    stack: List[ATenOp] = [root]
    while stack:
        n = stack.pop()
        k = id(n)
        if k in seen:
            continue
        seen.add(k)
        nid = ids[k]
        label = type(n).__name__
        if show_shape and n.T is not None:
            label += f"\\n{_dtype_str(n)} {_shape_str(n)}"
        if isinstance(n, ir.Const):
            label += f"\\nval={expr_to_str(n)}"
        nodes.append(f'n{nid} [label="{label}", shape=box];')

        for c in _children(n, include_predecessors=include_predecessors):
            cid = ids[id(c)]
            edges.append(f"n{cid} -> n{nid};")
            stack.append(c)

    body = "\n  ".join(nodes + edges)
    return 'digraph ATenOp {\n  rankdir=TB;\n  node [fontname="monospace"];\n  edge [fontname="monospace"];\n  ' + body + "\n}\n"

def get_jupyter_graphviz(dot_source: str) -> Any:
    try:
        import graphviz  # type: ignore
        return graphviz.Source(dot_source)
    except ImportError:
        class HtmlDot:
            def __init__(self, src: str) -> None: self.src = src
            def _repr_html_(self) -> str:
                return f"<div style='border:1px solid #ccc; padding:10px; white-space:pre; font-family:monospace'>{self.src}</div><div style='color:red'>graphviz module not found. try %pip install graphviz</div>"
        return HtmlDot(dot_source)
