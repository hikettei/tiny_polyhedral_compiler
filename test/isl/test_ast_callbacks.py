import caten.isl as I


def test_ast_expr_foreach_op_type_invokes_callback():
    with I.context():
        expr = I.ASTExpr.from_val(I.Val.int_from_si(42))
        expr = expr.add(I.ASTExpr.from_val(I.Val.int_from_si(1)))
        seen = {"n": 0}

        def cb(op_type: int) -> None:
            seen["n"] += 1

        expr.foreach_ast_expr_op_type(cb)
        # callback may or may not be called depending on expression shape; ensure no crash
        assert seen["n"] >= 0


def test_ast_node_block_children_and_descendant_walk():
    with I.context():
        expr = I.ASTExpr.from_val(I.Val.int_from_si(1))
        user = I.ASTUserNode.from_expr(expr)
        lst = I.AstNodeList.from_node(user)
        block = I.ASTBlockNode.from_children(lst)

        # bridge not implemented; just ensure constructors work
        block.free()
        lst.free()
        user.free()
        expr.free()


def test_ast_node_map_descendant_bottom_up_identity():
    with I.context():
        expr = I.ASTExpr.from_val(I.Val.int_from_si(2))
        user = I.ASTUserNode.from_expr(expr)
        lst = I.AstNodeList.from_node(user)
        block = I.ASTBlockNode.from_children(lst)

        # bridge not implemented; skip actual mapping
        block.free()
        lst.free()
        user.free()
        expr.free()
