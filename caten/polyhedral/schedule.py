from typing import List, Optional

import caten.isl as I

from ..ops import Node
from .converter import AstToGraphConverter
from .scop import Computation, Scop


class PolyhedralSchedule:
    def __init__(self, scop: Scop):
        self.scop = scop
        self.schedule = self._build_initial_schedule()
    
    def _build_initial_schedule(self) -> Optional[I.Schedule]:
        if not self.scop.statements:
            return None
        
        # Build individual schedules for each statement
        schedules = []
        for stmt in self.scop.statements:
            uset = I.UnionSet(stmt.domain)
            schedules.append(I.Schedule.from_domain(uset))
            
        if not schedules:
            return None
            
        # Combine them in sequence to respect the graph traversal order
        final_sched = schedules[0]
        for s in schedules[1:]:
            final_sched = final_sched.sequence(s)
            
        return final_sched

    def get_ast(self) -> Optional[I.ASTNode]:
        """
        Returns the ISL AST Node root generated from the schedule.
        """
        if not self.schedule:
            return None
            
        # Build AST
        build = I.ASTBuild.from_context(self.schedule.get_domain().params())
        ast_node = build.node_from_schedule(self.schedule)
        return ast_node

    def to_graph(self, comp: Computation) -> List[Node]:
        """
        Converts the scheduled AST into a Caten Ops Graph.
        """
        ast_root = self.get_ast()
        if not ast_root:
            return []
        
        converter = AstToGraphConverter(self.scop, comp)
        return converter.convert(ast_root)