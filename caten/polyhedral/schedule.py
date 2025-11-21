from typing import List, Optional

import caten.isl as I

from ..ops import ControlOps, MemoryOps, Node
from .converter import AstToGraphConverter
from .scop import Computation, Scop


class PolyhedralSchedule:
    def __init__(self, scop: Scop, graph: List[Node]):
        self.scop = scop
        self.graph = graph
        self.stmt_info_map = {s.name: s for s in scop.statements}
        self.schedule = self._compute_schedule()
    
    def _compute_schedule(self) -> Optional[I.Schedule]:
        if not self.graph:
            return None
        # Build schedule bottom-up from the graph
        return self._build_schedule_from_nodes(self.graph)

    def _build_schedule_from_nodes(self, nodes: List[Node]) -> Optional[I.Schedule]:
        schedules = []
        
        for node in nodes:
            if node.op == MemoryOps.STORE:
                if node in self.scop.node_to_id:
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
                        params_list = sorted(list(self.scop.params))
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
                # (Simplified: treating THEN block as sequence)
                then_b = node.arg[1]
                then_sched = self._build_schedule_from_nodes(then_b)
                if then_sched:
                    # TODO: Insert Guard filter
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
        """Recursively collect all STORE statement names inside a block/node."""
        stmts = []
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
        build = I.ASTBuild.from_context(self.schedule.get_domain().params())
        ast_node = build.node_from_schedule(self.schedule)
        return ast_node

    def to_graph(self, comp: Computation) -> List[Node]:
        ast_root = self.get_ast()
        if not ast_root:
            return []
        converter = AstToGraphConverter(self.scop, comp)
        return converter.convert(ast_root)

    def finalize(self, comp: Computation) -> str:
        raise NotImplementedError("Use get_ast() and ASTVisitor instead.")