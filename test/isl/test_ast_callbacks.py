from typing import Any

import pytest

import caten.isl as I


@pytest.mark.skip(reason="Requires manual callback binding support")
def test_ast_expr_foreach_op_type_invokes_callback() -> None:
    with I.context():
        expr = I.ASTExpr.from_val(I.Val.int_from_si(42))
        expr = expr.add(I.ASTExpr.from_val(I.Val.int_from_si(1)))
        seen = {"n": 0}

        def cb(op_type: int) -> None:
            seen["n"] += 1

        expr.foreach_ast_expr_op_type(cb)
        assert seen["n"] == 3

@pytest.mark.skip(reason="Requires ASTUserNode manual support")
def test_ast_node_block_children_and_descendant_walk() -> None:
    with I.context():
        expr = I.ASTExpr.from_val(I.Val.int_from_si(1))
        user = I.ASTUserNode.from_expr(expr)
        lst = I.AstNodeList.from_node(user)
        block = I.ASTNode.block_from_children(lst)
        
        seen = {"n": 0}
        def cb(node: Any) -> bool:
            seen["n"] += 1
            return True
            
        block.foreach_descendant_top_down(cb)
        assert seen["n"] == 2 # block + user

@pytest.mark.skip(reason="Requires ASTUserNode manual support")
def test_ast_node_map_descendant_bottom_up_identity() -> None:
    with I.context():
        expr = I.ASTExpr.from_val(I.Val.int_from_si(2))
        user = I.ASTUserNode.from_expr(expr)
        lst = I.AstNodeList.from_node(user)
        block = I.ASTNode.block_from_children(lst)
        
        def cb(node: Any) -> Any:
            return node
            
        block2 = block.map_descendant_bottom_up(cb)
        assert str(block2) == str(block)
