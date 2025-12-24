import caten.isl as I


def test_ast_expr_indexing_and_assign() -> None:
    with I.context():
        # Create identifier AST expressions
        A = I.expr("A")
        i = I.expr("i")
        B = I.expr("B")

        # Test __getitem__ (should call access)
        access_expr = A[i]
        assert access_expr.get_type() == I.specs.enums.isl_ast_expr_type.ISL_AST_EXPR_OP
        assert (
            access_expr.get_op_type() == I.specs.enums.isl_ast_expr_op_type.ISL_AST_EXPR_OP_ACCESS
        )

        # Check operands
        # Operand 0 is the array ID
        arr_id_expr = access_expr.get_op_arg(0)
        assert arr_id_expr.get_type() == I.specs.enums.isl_ast_expr_type.ISL_AST_EXPR_ID
        assert arr_id_expr.get_id().name() == "A"

        # Operand 1 is the index
        idx_expr = access_expr.get_op_arg(1)
        assert idx_expr.get_type() == I.specs.enums.isl_ast_expr_type.ISL_AST_EXPR_ID
        assert idx_expr.get_id().name() == "i"

        # Test assign
        # B[i]
        rhs_expr = B[i]

        # A[i].assign(B[i])
        node = access_expr.assign(rhs_expr)

        # Check node type
        assert node.get_type() == I.specs.enums.isl_ast_node_type.ISL_AST_NODE_USER

        # Check user expression
        user_expr = node.user_get_expr()
        assert user_expr.get_type() == I.specs.enums.isl_ast_expr_type.ISL_AST_EXPR_OP
        assert user_expr.get_op_type() == I.specs.enums.isl_ast_expr_op_type.ISL_AST_EXPR_OP_CALL

        # Check call function name "="
        func_expr = user_expr.get_op_arg(0)
        assert func_expr.get_id().name() == "assign"

        # Check arguments (A[i], B[i])
        arg1 = user_expr.get_op_arg(1)
        assert arg1.is_equal(access_expr)
        arg2 = user_expr.get_op_arg(2)
        assert arg2.is_equal(rhs_expr)


def test_ast_expr_operators() -> None:
    with I.context():
        a = I.expr("a")
        b = I.expr("b")

        # Test addition
        add_expr = a + b
        assert add_expr.get_op_type() == I.specs.enums.isl_ast_expr_op_type.ISL_AST_EXPR_OP_ADD

        # Test mul
        mul_expr = a * b
        assert mul_expr.get_op_type() == I.specs.enums.isl_ast_expr_op_type.ISL_AST_EXPR_OP_MUL
