from __future__ import annotations
from typing import Any, List, Optional, Tuple, Union, Dict, Callable

from ctypes import CFUNCTYPE, c_void_p, cast, py_object
from caten.isl.specs.enums import isl_ast_node_type, isl_ast_expr_type, isl_ast_expr_op_type

import caten.isl as I

class Directive():
    pass

# [TODO]
# Separate AST Generation and Rendering Process
# Use MLIR?
def schedule_to_ast(schedule: I.Schedule, stmts: Dict[str, Callable]):
    ast_node = I.ASTBuild.alloc().node_from_schedule(schedule)
    # Callback signature: isl_ast_node *(*fn)(__isl_take isl_ast_node *node, void *user)
    CALLBACK = CFUNCTYPE(c_void_p, c_void_p, c_void_p)
    def replace_cb(node_handle, user_data):
        node = I.ASTNode(node_handle)    
        if node.get_type() == isl_ast_node_type.ISL_AST_NODE_USER: 
            expr = node.user_get_expr()
            if expr.get_type() == isl_ast_expr_type.ISL_AST_EXPR_OP: 
                if expr.get_op_type() == isl_ast_expr_op_type.ISL_AST_EXPR_OP_CALL: 
                    call_id = expr.get_op_arg(0).id_get_id()
                    name = call_id.name()
                    if name in stmts:
                        func = stmts[name]
                        n_args = expr.get_op_n_arg()
                        # arg 0 is the function ID, actual args start from 1
                        args = [expr.get_op_arg(i) for i in range(1, n_args)]
                        new_node = func(*args)
                        return new_node.copy_handle()
        return node.copy_handle()
    c_cb = CALLBACK(replace_cb)
    cb_ptr = cast(c_cb, c_void_p)
    ast_node = ast_node.map_descendant_bottom_up(cb_ptr, None)
    return ast_node

def schedule_to_c(schedule: I.Schedule, stmts: Dict[str, Callable]):
    # Print to C
    p = I.Printer.alloc_str()
    p.request_inplace()
    p = p.set_output_format(4) # ISL_FORMAT_C   
    p.request_inplace()
    p = p.print_ast_node(schedule_to_ast(schedule, stmts))
    return p.get_str()
