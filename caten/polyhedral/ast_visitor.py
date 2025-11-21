from abc import ABC, abstractmethod

import caten.isl as I


class ASTNodeType:
    ERROR = -1
    FOR = 1
    IF = 2
    BLOCK = 3
    MARK = 4
    USER = 5

class ASTVisitor(ABC):
    def visit(self, node: I.ASTNode):
        # Handle None or mismatched types gracefully?
        # ISL AST nodes are wrapped.
        ntype = node.get_type()
        
        if ntype == ASTNodeType.FOR:
            return self.visit_for(node)
        elif ntype == ASTNodeType.IF:
            return self.visit_if(node)
        elif ntype == ASTNodeType.BLOCK:
            return self.visit_block(node)
        elif ntype == ASTNodeType.USER:
            return self.visit_user(node)
        elif ntype == ASTNodeType.MARK:
            return self.visit_mark(node)
        else:
            # Fallback or error
            return self.visit_generic(node)

    @abstractmethod
    def visit_for(self, node: I.ASTNode): pass
    
    @abstractmethod
    def visit_if(self, node: I.ASTNode): pass
    
    @abstractmethod
    def visit_block(self, node: I.ASTNode): pass
    
    @abstractmethod
    def visit_user(self, node: I.ASTNode): pass
    
    def visit_mark(self, node: I.ASTNode):
        # Default implementation: skip mark and visit child
        return self.visit(node.mark_get_node())

    def visit_generic(self, node: I.ASTNode):
        raise NotImplementedError(f"Unhandled AST node type: {node.get_type()}")