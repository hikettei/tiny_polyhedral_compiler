from __future__ import annotations
from typing import Any, List, Optional, Tuple, Union
import caten.isl as I

class DotVisualizer:
    def __init__(self, title: str):
        self.lines = [
            f'digraph "{title}" {{',
            'node [fontname="Helvetica", fontsize=10, shape=box, style="rounded,filled", color="#333333", fillcolor="#F5F5F5"];',
            'edge [fontname="Helvetica", fontsize=9, color="#666666"];',
            'rankdir=TB;',
        ]
        self.node_counter = 0

    def add_node(self, label: str, **attrs) -> str:
        node_id = f"node_{self.node_counter}"
        self.node_counter += 1
        attr_str = ", ".join(f'{k}="{v}"' for k, v in attrs.items())
        
        if label.strip().startswith("<") and label.strip().endswith(">"):
            self.lines.append(f'{node_id} [label={label}, {attr_str}];')
        else:
            label = label.replace('"', '\"').replace('\n', '\\n')
            self.lines.append(f'{node_id} [label="{label}", {attr_str}];')
        return node_id

    def add_edge(self, src: str, dst: str, label: str = ""):
        attr_str = f'label="{label}"' if label else ""
        self.lines.append(f'{src} -> {dst} [{attr_str}];')

    def render(self):
        self.lines.append("}")
        return "\n".join(self.lines)

def _get_jupyter_graphviz(dot_source: str) -> Any:
    try:
        import graphviz
        return graphviz.Source(dot_source)
    except ImportError:
        class HtmlDot:
            def __init__(self, src): self.src = src
            def _repr_html_(self):
                return f"<div style='border:1px solid #ccc; padding:10px; white-space:pre; font-family:monospace'>{self.src}</div><div style='color:red'>graphviz module not found. try %pip install graphviz</div>"
        return HtmlDot(dot_source)

def clean_math(s: str) -> str:
    """Replaces text operators with mathematical symbols."""
    replacements = {
        " and ": " ∧ ",
        " or ": " ∨ ",
        "exists": "∃",
        "implies": "⇒",
        ">=": "≥",
        "<=": "≤",
        "!=": "≠",
        " -> ": " → ",
    }
    for old, new in replacements.items():
        s = s.replace(old, new)
    return s

def split_at_top_level(s: str, separators: List[str]) -> List[str]:
    """
    Splits a string at the given separators, but only if not nested inside [], {}, ().
    """
    res = []
    current = []
    depth = 0
    i = 0
    while i < len(s):
        char = s[i]
        
        # Check for separators
        is_sep = False
        for sep in separators:
            if s.startswith(sep, i):
                if depth == 0:
                    res.append("".join(current).strip())
                    current = []
                    i += len(sep)
                    is_sep = True
                    break
        
        if is_sep:
            continue
            
        if char in "[{(":
            depth += 1
        elif char in "]})":
            depth -= 1
            
        current.append(char)
        i += 1
        
    if current:
        res.append("".join(current).strip())
    return [r for r in res if r]

def parse_isl_object(obj: Any) -> Tuple[str, List[str]]:
    """
    Parses an ISL object (Set, UnionMap, etc.) into (params_str, body_items).
    Removes outer braces.
    """
    s = str(obj)
    params = ""
    body = s
    
    # Extract params: "[N, K] -> { ... }"
    if " -> {" in s:
        parts = s.split(" -> {", 1)
        params = parts[0].strip("[]")
        body = parts[1].strip("} ")
    elif s.startswith("{") and s.endswith("}"):
        body = s[1:-1].strip()
    elif s.startswith("[") and " -> " in s: # Handle cases like "[N] -> S[N]"
        parts = s.split(" -> ", 1)
        params = parts[0].strip("[]")
        body = parts[1]

    # Clean body
    body = clean_math(body)
    
    # Split items
    # UnionMaps/Sets usually use "; "
    # But sometimes simple Sets use ", " for constraints, we don't want to split that.
    # We primarily want to split the top-level entities (e.g., S1; S2).
    if "; " in body:
        items = split_at_top_level(body, ["; "])
    else:
        # If it's a list representation (like MultiUnionPwAff sometimes), check logic
        # For now, treat as single item unless semicolon found
        items = [body]
        
    return params, items

def print_schedule(node: "I.ScheduleNode") -> str:
    """
    CLI visualization of the schedule tree.
    """
    output_lines = []

    def _clean_item(item: str) -> str:
        # Remove [(...)] wrapping in RHS of arrow if present
        if " → " in item:
            lhs, rhs = item.split(" → ", 1)
            rhs = rhs.strip()
            if rhs.startswith("[(") and rhs.endswith(")]"):
                rhs = rhs[2:-2]
            elif rhs.startswith("[") and rhs.endswith("]"):
                rhs = rhs[1:-1]
            if rhs.startswith("(") and rhs.endswith(")"):
                rhs = rhs[1:-1]
            return f"{lhs} → {rhs}"
        return item

    def _align_items(items: List[str], separator: str) -> List[str]:
        if not items or not any(separator in item for item in items):
            return items
            
        max_lhs_len = 0
        split_data = []
        
        for item in items:
            if separator in item:
                lhs, rhs = item.split(separator, 1)
                lhs = lhs.strip()
                rhs = rhs.strip()
                max_lhs_len = max(max_lhs_len, len(lhs))
                split_data.append((lhs, rhs))
            else:
                split_data.append((item, None))
        
        aligned = []
        for lhs, rhs in split_data:
            if rhs is not None:
                aligned.append(f"{lhs.ljust(max_lhs_len)} {separator} {rhs}")
            else:
                aligned.append(lhs)
        return aligned

    def _rec(n: "I.ScheduleNode", indent: str):
        t_name = n.get_type_name()
        
        # 1. Extract Data
        header_text = f"[{t_name}]"
        items = []
        
        if t_name == "domain":
            params, body = parse_isl_object(n.domain_get_domain())
            if params:
                header_text += f" ({params})"
            items = body
        
        elif t_name == "band":
            mupa = n.band_get_partial_schedule()
            n_dims = mupa.dim(3) # isl_dim_set / isl_dim_out
            for i in range(n_dims):
                upa = mupa.get_union_pw_aff(i)
                _, body = parse_isl_object(upa)
                items.extend([_clean_item(b) for b in body])
                
        elif t_name == "filter":
            params, body = parse_isl_object(n.filter_get_filter())
            items = body
            
        elif t_name == "sequence":
            pass # Header is generated automatically by t_name
            
        elif t_name == "set":
            pass # Header is generated automatically by t_name
            
        elif t_name == "mark":
            items = [f'"{n.mark_get_id().name()}"']
            
        elif t_name == "context":
            params, body = parse_isl_object(n.context_get_context())
            items = body
            
        elif t_name == "guard":
            params, body = parse_isl_object(n.guard_get_guard())
            items = body
            
        elif t_name == "extension":
            params, body = parse_isl_object(n.extension_get_extension())

        # 2. Render Node
        
        # Align items if applicable
        separator = None
        if t_name in ["domain", "filter"]:
            separator = ":"
        elif t_name == "band":
            separator = "→"
            
        if separator:
            items = _align_items(items, separator)

        has_items = len(items) > 0
        
        # Header
        marker = "┏ " if has_items else "┗ "
        if node.is_equal(n):
            header_text += " (You are here)"
        output_lines.append(f"{indent}{marker}{header_text}")
        
        # Items
        if has_items:
            for i, item in enumerate(items):
                is_last_item = (i == len(items) - 1)
                item_marker = "┗ " if is_last_item else "┃ "
                output_lines.append(f"{indent}{item_marker} {item}")

        # 3. Recurse Children
        child_indent = indent + "  "
        n_children = n.n_children()
        for i in range(n_children):
            _rec(n.child(i), child_indent)

    # Root wrapper
    root_type = node.get_type_name()
    
    _rec(node, "    ")
    return "\n".join(output_lines)

def viz_schedule(node: Union["I.ScheduleNode", "P.ScheduleNodeBase"]) -> Any:
    if hasattr(node, "get_node"):
        node = node.get_node()
    
    viz = DotVisualizer("ScheduleTree")
    import html
    
    def _clean_item_viz(item: str) -> str:
        if " → " in item:
            lhs, rhs = item.split(" → ", 1)
            rhs = rhs.strip()
            if rhs.startswith("[(") and rhs.endswith(")]"):
                rhs = rhs[2:-2]
            elif rhs.startswith("[") and rhs.endswith("]"):
                rhs = rhs[1:-1]
            if rhs.startswith("(") and rhs.endswith(")"):
                rhs = rhs[1:-1]
            return f"{lhs} → {rhs}"
        return item

    def _rec(n: "I.ScheduleNode", parent_id: Optional[str] = None):
        t_name = n.get_type_name()
        fillcolor = "#FFFFFF"
        
        items = []
        params = ""
        
        if t_name == "domain":
            params, items = parse_isl_object(n.domain_get_domain())
            fillcolor = "#E3F2FD"
        elif t_name == "band":
            mupa = n.band_get_partial_schedule()
            n_dims = mupa.dim(3)
            for i in range(n_dims):
                upa = mupa.get_union_pw_aff(i)
                _, body = parse_isl_object(upa)
                items.extend([_clean_item_viz(b) for b in body])
            fillcolor = "#E8F5E9"
        elif t_name == "filter":
            params, items = parse_isl_object(n.filter_get_filter())
            fillcolor = "#FFF3E0"
        elif t_name == "sequence":
            fillcolor = "#F3E5F5"
        elif t_name == "set":
            fillcolor = "#F3E5F5"
        elif t_name == "mark":
            items = [f'"{n.mark_get_id().name()}"']
            fillcolor = "#FFF8E1"
        elif t_name == "context":
            params, items = parse_isl_object(n.context_get_context())
            fillcolor = "#ECEFF1"
        elif t_name == "guard":
            params, items = parse_isl_object(n.guard_get_guard())
            fillcolor = "#FFEBEE"
            
        # Build HTML Label
        rows = []
        header_text = f"[{t_name}]"
        if params: header_text += f" ({params})"
        header_text = html.escape(header_text)
        
        separator = None
        if t_name in ["domain", "filter"]:
            separator = ":"
        elif t_name == "band":
            separator = "→"
            
        use_two_cols = False
        if items and separator:
            use_two_cols = any(separator in item for item in items)
            
        colspan = ' COLSPAN="2"' if use_two_cols else ''
        rows.append(f'<TR><TD ALIGN="LEFT" BALIGN="LEFT"{colspan}><B>{header_text}</B></TD></TR>')
        
        for item in items:
            clean_item = html.escape(item)
            if use_two_cols and separator in item:
                parts = item.split(separator, 1)
                left = html.escape(parts[0].strip())
                right = html.escape(parts[1].strip())
                sep_esc = html.escape(separator)
                rows.append(f'<TR><TD ALIGN="LEFT" BALIGN="LEFT">{left}</TD><TD ALIGN="LEFT" BALIGN="LEFT">{sep_esc} {right}</TD></TR>')
            elif use_two_cols:
                 rows.append(f'<TR><TD ALIGN="LEFT" BALIGN="LEFT" COLSPAN="2">{clean_item}</TD></TR>')
            else:
                 rows.append(f'<TR><TD ALIGN="LEFT" BALIGN="LEFT">{clean_item}</TD></TR>')
        
        label = f'<<TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0" CELLPADDING="2">{"".join(rows)}</TABLE>>'
        
        current_id = viz.add_node(label, fillcolor=fillcolor)
        
        if parent_id:
            viz.add_edge(parent_id, current_id)
            
        n_children = n.n_children()
        for i in range(n_children):
            _rec(n.child(i), current_id)

    _rec(node)
    return _get_jupyter_graphviz(viz.render())

def viz_ast(node: "I.ASTNode") -> Any:
    viz = DotVisualizer("AST")
    from caten.isl.specs.enums import isl_ast_node_type
    
    def _rec(n: "I.ASTNode", parent_id: Optional[str] = None, edge_label: str = ""):
        ntype = n.get_type()
        label = ""
        fillcolor = "#FFFFFF"
        shape = "box"
        
        if ntype == isl_ast_node_type.ISL_AST_NODE_FOR:
            it = n.for_get_iterator()
            init = n.for_get_init()
            cond = n.for_get_cond()
            inc = n.for_get_inc()
            label = f"FOR {it}\nInit: {init}\nCond: {cond}\nInc: {inc}"
            fillcolor = "#E3F2FD"
            current_id = viz.add_node(label, fillcolor=fillcolor)
            if parent_id: viz.add_edge(parent_id, current_id, edge_label)
            
            _rec(n.for_get_body(), current_id)
            
        elif ntype == isl_ast_node_type.ISL_AST_NODE_IF:
            cond = n.if_get_cond()
            label = f"IF\n{cond}"
            fillcolor = "#FFEBEE"
            shape = "diamond"
            current_id = viz.add_node(label, shape=shape, fillcolor=fillcolor)
            if parent_id: viz.add_edge(parent_id, current_id, edge_label)
            
            _rec(n.if_get_then(), current_id, "True")
            if n.if_has_else():
                _rec(n.if_get_else(), current_id, "False")
                
        elif ntype == isl_ast_node_type.ISL_AST_NODE_BLOCK:
            label = "BLOCK"
            fillcolor = "#ECEFF1"
            current_id = viz.add_node(label, fillcolor=fillcolor)
            if parent_id: viz.add_edge(parent_id, current_id, edge_label)
            
            children = n.block_get_children()
            count = children.n_ast_node()
            for i in range(count):
                _rec(children.get_at(i), current_id)
                
        elif ntype == isl_ast_node_type.ISL_AST_NODE_MARK:
            mk = n.mark_get_id()
            label = f"MARK\n{mk.name()}"
            fillcolor = "#FFF8E1"
            current_id = viz.add_node(label, fillcolor=fillcolor)
            if parent_id: viz.add_edge(parent_id, current_id, edge_label)
            _rec(n.mark_get_node(), current_id)
            
        elif ntype == isl_ast_node_type.ISL_AST_NODE_USER:
            expr = n.user_get_expr()
            label = f"USER\n{expr.to_C_str()}"
            fillcolor = "#E8F5E9"
            current_id = viz.add_node(label, fillcolor=fillcolor)
            if parent_id: viz.add_edge(parent_id, current_id, edge_label)
            
        else:
            label = "UNKNOWN"
            current_id = viz.add_node(label)
            if parent_id: viz.add_edge(parent_id, current_id, edge_label)

    _rec(node)
    return _get_jupyter_graphviz(viz.render())
