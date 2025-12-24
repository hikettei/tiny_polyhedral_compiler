from enum import IntEnum


class isl_error(IntEnum):
    ISL_ERROR_NONE = 0
    ISL_ERROR_ABORT = 1
    ISL_ERROR_ALLOC = 2
    ISL_ERROR_UNKNOWN = 3
    ISL_ERROR_INTERNAL = 4
    ISL_ERROR_INVALID = 5
    ISL_ERROR_QUOTA = 6
    ISL_ERROR_UNSUPPORTED = 7

class isl_stat(IntEnum):
    ISL_STAT_ERROR = -1
    ISL_STAT_OK = 0

class isl_bool(IntEnum):
    ISL_BOOL_ERROR = -1
    ISL_BOOL_FALSE = 0
    ISL_BOOL_TRUE = 1

class isl_lp_result(IntEnum):
    ISL_LP_ERROR = -1
    ISL_LP_OK = 0
    ISL_LP_UNBOUNDED = 1
    ISL_LP_EMPTY = 2

class isl_fold(IntEnum):
    ISL_FOLD_ERROR = -1
    ISL_FOLD_MIN = 0
    ISL_FOLD_MAX = 1
    ISL_FOLD_LIST = 2

class isl_schedule_node_type(IntEnum):
    ISL_SCHEDULE_NODE_ERROR = -1
    ISL_SCHEDULE_NODE_BAND = 0
    ISL_SCHEDULE_NODE_CONTEXT = 1
    ISL_SCHEDULE_NODE_DOMAIN = 2
    ISL_SCHEDULE_NODE_EXPANSION = 3
    ISL_SCHEDULE_NODE_EXTENSION = 4
    ISL_SCHEDULE_NODE_FILTER = 5
    ISL_SCHEDULE_NODE_LEAF = 6
    ISL_SCHEDULE_NODE_GUARD = 7
    ISL_SCHEDULE_NODE_MARK = 8
    ISL_SCHEDULE_NODE_SEQUENCE = 9
    ISL_SCHEDULE_NODE_SET = 10

class isl_ast_node_type(IntEnum):
    ISL_AST_NODE_ERROR = -1
    ISL_AST_NODE_FOR = 1
    ISL_AST_NODE_IF = 2
    ISL_AST_NODE_BLOCK = 3
    ISL_AST_NODE_MARK = 4
    ISL_AST_NODE_USER = 5

class isl_ast_expr_type(IntEnum):
    ISL_AST_EXPR_ERROR = -1
    ISL_AST_EXPR_OP = 0
    ISL_AST_EXPR_ID = 1
    ISL_AST_EXPR_INT = 2

class isl_ast_expr_op_type(IntEnum):
    ISL_AST_EXPR_OP_ERROR = -1
    ISL_AST_EXPR_OP_AND = 0
    ISL_AST_EXPR_OP_AND_THEN = 1
    ISL_AST_EXPR_OP_OR = 2
    ISL_AST_EXPR_OP_OR_ELSE = 3
    ISL_AST_EXPR_OP_MAX = 4
    ISL_AST_EXPR_OP_MIN = 5
    ISL_AST_EXPR_OP_MINUS = 6
    ISL_AST_EXPR_OP_ADD = 7
    ISL_AST_EXPR_OP_SUB = 8
    ISL_AST_EXPR_OP_MUL = 9
    ISL_AST_EXPR_OP_DIV = 10
    ISL_AST_EXPR_OP_FDIV_Q = 11
    ISL_AST_EXPR_OP_PDIV_Q = 12
    ISL_AST_EXPR_OP_PDIV_R = 13
    ISL_AST_EXPR_OP_ZDIV_R = 14
    ISL_AST_EXPR_OP_COND = 15
    ISL_AST_EXPR_OP_SELECT = 16
    ISL_AST_EXPR_OP_EQ = 17
    ISL_AST_EXPR_OP_LE = 18
    ISL_AST_EXPR_OP_LT = 19
    ISL_AST_EXPR_OP_GE = 20
    ISL_AST_EXPR_OP_GT = 21
    ISL_AST_EXPR_OP_CALL = 22
    ISL_AST_EXPR_OP_ACCESS = 23
    ISL_AST_EXPR_OP_MEMBER = 24
    ISL_AST_EXPR_OP_ADDRESS_OF = 25

class isl_ast_loop_type(IntEnum):
    ISL_AST_LOOP_ERROR = -1
    ISL_AST_LOOP_DEFAULT = 0
    ISL_AST_LOOP_ATOMIC = 1
    ISL_AST_LOOP_UNROLL = 2
    ISL_AST_LOOP_SEPARATE = 3

class isl_dim_type(IntEnum):
    ISL_DIM_CST = 0
    ISL_DIM_PARAM = 1
    ISL_DIM_IN = 2
    ISL_DIM_OUT = 3
    ISL_DIM_SET = 3
    ISL_DIM_DIV = 4
    ISL_DIM_ALL = 5

# Mappings for get_type_name
_ISL_SCHEDULE_NODE_TYPE_MAP = {
    -1: "error",
    0: "band",
    1: "context",
    2: "domain",
    3: "expansion",
    4: "extension",
    5: "filter",
    6: "leaf",
    7: "guard",
    8: "mark",
    9: "sequence",
    10: "set",
}

_ISL_AST_NODE_TYPE_MAP = {
    -1: "error",
    1: "for",
    2: "if",
    3: "block",
    4: "mark",
    5: "user",
}

_ISL_AST_EXPR_TYPE_MAP = {
    -1: "error",
    0: "op",
    1: "id",
    2: "int",
}

_ISL_AST_EXPR_OP_TYPE_MAP = {
    -1: "error",
    0: "and",
    1: "and_then",
    2: "or",
    3: "or_else",
    4: "max",
    5: "min",
    6: "minus",
    7: "add",
    8: "sub",
    9: "mul",
    10: "div",
    11: "fdiv_q",
    12: "pdiv_q",
    13: "pdiv_r",
    14: "zdiv_r",
    15: "cond",
    16: "select",
    17: "eq",
    18: "le",
    19: "lt",
    20: "ge",
    21: "gt",
    22: "call",
    23: "access",
    24: "member",
    25: "address_of",
}

_ISL_FOLD_MAP = {
    -1: "error",
    0: "min",
    1: "max",
    2: "list",
}