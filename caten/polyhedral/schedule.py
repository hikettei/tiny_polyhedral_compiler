from typing import List, Dict, Any, Optional, Tuple
import caten.isl as I
from .scop import Scop, Computation
from .converter import AstToGraphConverter
from ..ops import Node, OpType, ControlOps, MemoryOps
import re

class PolyhedralSchedule:
    def __init__(self, scop: Optional[Scop] = None, graph: Optional[List[Node]] = None, schedule: Optional[I.Schedule] = None):
        self.scop = scop
        self.graph = graph
        self.stmt_info_map = {s.name: s for s in scop.statements} if scop else {}
        
        if schedule:
            self.schedule = schedule
        elif scop and graph:
            self.schedule = self._compute_schedule()
        else:
            self.schedule = None
    
    def _compute_schedule(self) -> Optional[I.Schedule]:
        if not self.graph or not self.scop:
            return None
        # Build schedule bottom-up from the graph
        return self._build_schedule_from_nodes(self.graph)

    def _build_schedule_from_nodes(self, nodes: List[Node]) -> Optional[I.Schedule]:
        schedules = []
        
        for node in nodes:
            if node.op == MemoryOps.STORE:
                if self.scop and node in self.scop.node_to_id:
                    stmt_id = self.scop.node_to_id[node]
                    stmt_info = self.stmt_info_map.get(stmt_id)
                    if stmt_info:
                        uset = I.UnionSet(stmt_info.domain)
                        schedules.append(I.Schedule.from_domain(uset))
            
            elif node.op == ControlOps.RANGE:
                body = node.arg[2]
                body_sched = self._build_schedule_from_nodes(body)
                
                if body_sched:
                    # Insert Loop Band
                    iter_sym = node.arg[0]
                    stmts_in_loop = self._collect_statements(node)
                    mupa_parts = []
                    
                    for stmt_name in stmts_in_loop:
                        stmt_info = self.stmt_info_map.get(stmt_name)
                        if stmt_info:
                            try:
                                idx = stmt_info.iter_names.index(iter_sym.name)
                                vars_str = ", ".join(stmt_info.iter_names)
                                target_val = stmt_info.iter_names[idx]
                                mupa_parts.append(f"{stmt_name}[{vars_str}] -> [{target_val}]")
                            except ValueError:
                                pass
                    
                    if mupa_parts:
                        params_list = sorted(list(self.scop.params)) if self.scop else []
                        if params_list:
                            mupa_str = f"[{', '.join(params_list)}] -> {{ " + "; ".join(mupa_parts) + " }"
                        else:
                            mupa_str = "{ " + "; ".join(mupa_parts) + " }"
                        
                        try:
                            umap = I.UnionMap(mupa_str)
                            mupa = I.MultiUnionPwAff.from_union_map(umap)
                            body_sched = body_sched.insert_partial_schedule(mupa)
                        except Exception as e:
                            print(f"WARNING: Failed to insert partial schedule: {e} for {mupa_str}")
                    
                    schedules.append(body_sched)
            
            elif node.op == ControlOps.IF:
                # Recursively process IF body
                then_b = node.arg[1]
                then_sched = self._build_schedule_from_nodes(then_b)
                if then_sched:
                    schedules.append(then_sched)
                
                else_b = node.arg[2]
                if else_b:
                    else_sched = self._build_schedule_from_nodes(else_b)
                    if else_sched:
                        schedules.append(else_sched)

        if not schedules:
            return None
            
        # Combine sibling schedules with sequence
        final_sched = schedules[0]
        for s in schedules[1:]:
            final_sched = final_sched.sequence(s)
            
        return final_sched

    def _collect_statements(self, node: Node) -> List[str]:
        stmts = []
        if not self.scop: return stmts
        
        if node.op == MemoryOps.STORE:
            if node in self.scop.node_to_id:
                stmts.append(self.scop.node_to_id[node])
        elif node.op == ControlOps.RANGE:
            body = node.arg[2]
            for n in body:
                stmts.extend(self._collect_statements(n))
        elif node.op == ControlOps.IF:
            then_b = node.arg[1]
            else_b = node.arg[2]
            for n in then_b:
                stmts.extend(self._collect_statements(n))
            if else_b:
                for n in else_b:
                    stmts.extend(self._collect_statements(n))
            
        return stmts

    def get_ast(self) -> Optional[I.ASTNode]:
        if not self.schedule:
            return None
        
        # Use params from scop if available, else empty context
        if self.schedule.get_domain():
             ctx = self.schedule.get_domain().params()
             build = I.ASTBuild.from_context(ctx)
        else:
             build = I.ASTBuild.alloc()
             
        ast_node = build.node_from_schedule(self.schedule)
        return ast_node

    def to_graph(self, comp: Computation) -> List[Node]:
        ast_root = self.get_ast()
        if not ast_root:
            return []
        if not self.scop:
            return [] # Cannot convert without scop info
        converter = AstToGraphConverter(self.scop, comp)
        return converter.convert(ast_root)

    def finalize(self, comp: Computation) -> str:
        raise NotImplementedError("Use get_ast() and ASTVisitor instead.")

    def sequence(self, other: 'PolyhedralSchedule') -> 'PolyhedralSchedule':
        """
        Combine this schedule with another in a sequence.
        """
        if self.schedule is None or other.schedule is None:
             raise ValueError("Cannot sequence None schedules")
             
        new_sched = self.schedule.sequence(other.schedule)
        return PolyhedralSchedule(schedule=new_sched)

    def get_root(self) -> Any:
        """Returns the root ScheduleNode of the schedule."""
        if not self.schedule:
            return None
        return self.schedule.get_root()

    def update(self, node: Any) -> None:
        """Update the internal schedule from a ScheduleNode."""
        if hasattr(node, 'get_schedule'):
            self.schedule = node.get_schedule()
        else:
            # Assume it is a schedule
            self.schedule = node

    def is_legal(self) -> bool:
        """Check if the schedule respects dependencies."""
        # TODO: Implement actual dependency checking using access maps if available
        return True

    def to_c(self) -> str:
        """Generate C code from the schedule."""
        from .codegen import to_c
        return to_c(self.schedule)
