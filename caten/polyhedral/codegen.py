from typing import Optional

import caten.isl as I


def to_c(schedule: "I.Schedule", context: Optional["I.Context"] = None) -> str:
    """
    Generate C code from an ISL schedule.
    """
    if context is None:
        # Try to get context from schedule
        context = schedule.get_ctx()
    
    # Build AST
    build = I.ASTBuild.alloc()
    ast_node = build.node_from_schedule(schedule)
    
    # Print to C
    p = I.Printer.alloc_str()
    p.request_inplace()
    p = p.set_output_format(4) # ISL_FORMAT_C
    p.request_inplace()
    p = p.print_ast_node(ast_node)
    
    return p.get_str()