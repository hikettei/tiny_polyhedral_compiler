from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Callable, Dict, List, Optional, Sequence, Tuple

import caten.isl as I
from .analysis import compute_dependence_relation, compute_schedule_legality

class TraversalHelper():
    def __init__(self, node: "I.ScheduleNode"):
        self.node = node
    
    def current(self) -> Tuple[int, ...]:
        path: List[int] = []
        current = self.node
        while current.has_parent():
            path.append(current.get_child_position())
            current = current.parent()
        return tuple(reversed(path))
    
    def switch(self, path: Sequence[int]) -> "I.ScheduleNode":
        current = self.node
        for idx in path: current = current.child(idx)
        return current

    def filter(self, predicate: Callable[["I.ScheduleNode", Sequence[int]], bool]) -> List[List[int]]:
        results: List[List[int]] = []
        def explore(n: "I.ScheduleNode", path: List[int]) -> None:
            if predicate(n, path):
                results.append(list(path))
            for i in range(n.n_children()):
                explore(n.child(i), path + [i])
        explore(node, [])
        return results

@dataclass(frozen=True)
class OptRecord():
    f: Callable
    args: list
    kwargs: dict

class ConstraintedModel:
    def __init__(
        self,
        schedule: I.Schedule,
        read_maps: Optional[I.UnionMap],
        write_maps: Optional[I.UnionMap],
        stmts: Dict[str, Callable],
    ) -> None:
        self.schedule = schedule
        self.stmts = stmts
        self.read_umap = read_maps
        self.write_umap = write_maps
        self.deps, _, _, _ = compute_dependence_relation(read_maps, write_maps, schedule)
        self.history: List[OptRecord] = [] # TODO

    @classmethod
    def from_schedule(
        cls,
        schedule: I.Schedule,
        read_umap: Optional[I.UnionMap],
        write_umap: Optional[I.UnionMap],
        stmts: Dict[str, Callable],
    ) -> "ConstraintedModel":
        return cls(schedule, read_umap, write_umap, stmts)

    def editor(self) -> "Dispatcher": return Dispatcher(self.schedule.get_root(), self)

    def __add__(self, other: "ConstraintedModel") -> "ConstraintedModel":
        read_umap = self.read_umap | other.read_umap
        write_umap = self.write_umap | other.write_umap
        schedule = self.schedule.sequence(other.schedule)
        intersections = self.stmts.keys() & other.stmts.keys()
        assert len(intersections) == 0, (
            "Cannot add two models because they have common stmt keys: "
            f"{intersections}"
        )
        return ConstraintedModel.from_schedule(
            schedule, read_umap, write_umap, stmts=self.stmts | other.stmts
        )

    def update_schedule(self, new_schedule: I.Schedule):
        self.schedule = new_schedule

def transformation(body: Callable[Any, I.ScheduleNode]):
    def f(*args, **kwargs):
        new_schedule = body(*args, **kwargs)
        assert isinstance(new_schedule, I.ScheduleNode)
        assert isinstance(args[0], Dispatcher)
        return args[0].commit(f"{body.__name__}{args[1:]}", new_schedule)
    return f

class Dispatcher:
    def __init__(
        self,
        node: I.ScheduleNode,
        model: ConstraintedModel,
    ) -> None:
        self.model = model
        self.path = TraversalHelper(node).current()

    def commit(self, name: str, new_schedule: I.ScheduleNode) -> I.ScheduleNode:
        if not compute_schedule_legality(new_schedule.get_schedule(), self.model.deps):
            raise RuntimeError(
                f"Detected illegal loop transformation: {name}."
                f"\n{self.__repr__()}"
            )
        # [TODO] Update records
        self.model.update_schedule(new_schedule.get_schedule())
        return self
    
    @property
    def node(self) -> I.ScheduleNode:
        return TraversalHelper(self.model.schedule.get_root()).switch(self.path)

    def to_c(self) -> str:
        from caten.polyhedral.codegen import schedule_to_c
        return schedule_to_c(self.node.get_schedule(), self.model.stmts)

    def __repr__(self) -> str:
        from caten.polyhedral.viz import print_schedule
        return f"{self.__class__.__name__}(\n{print_schedule(self.node)}\n)"

    def __enter__(self) -> "Dispatcher":
        # TODO: Selfより下に存在するScheduleNodeしか触ってはいけない
        return self

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        # TODO: 元のPathまで戻ってくる
        return None

    def ensure_and_dispatch(self, cls: type["Dispatcher"], expect: str) -> "Dispatcher":
        if not self.node.get_type_name() == expect:
            raise RuntimeError(
                f"Cannot switch to the {expect}, you are now at {self.node.get_type_name()}!.\n"
                f"{self.__repr__()}"
            )
        return cls(self.node, self.model)

    def domain(self) -> "DomainEditor":
        return self.ensure_and_dispatch(DomainEditor, "domain")

    def band(self) -> "BandEditor":
        return self.ensure_and_dispatch(BandEditor, "band")

    def filter(self) -> "FilterEditor":
        return self.ensure_and_dispatch(FilterEditor, "filter")

    def sequence(self) -> "SequenceEditor":
        return self.ensure_and_dispatch(SequenceEditor, "sequence")

    def set(self) -> "SetEditor":
        return self.ensure_and_dispatch(SetEditor, "set")

    def __getitem__(self, key: int) -> "Dispatcher":
        return Dispatcher(self.node.child(key), self.model)

    def mark(self, directive: Any) -> I.ScheduleNode:
        # [TODO]
        raise TypeError("mark expects a Directive or string id.")

class DomainEditor(Dispatcher):
    pass

class FilterEditor(Dispatcher):
    pass

class BandEditor(Dispatcher):
    def get_tiling_sizes(self, sizes: int | List[int]) -> I.MultiVal:
        depth = self.node.band_get_space().dim(3)
        sizes = [sizes] * depth if isinstance(sizes, int) else sizes
        if not len(sizes) == depth:
            raise ValueError(
                f"Tiling size mismatch: Band depth is {depth}, "
                f"but provided {len(sizes)} sizes: {sizes}."
            )
        mv = I.MultiVal.zero(self.node.band_get_space())
        for i, size in enumerate(sizes):
            mv = mv.set_val(i, I.Val.int_from_si(int(size)))
        return mv

    @property
    def depth(self) -> int:
        return self.node.band_get_space().dim(3)

    @transformation
    def scale(self, sizes: int | List[int]) -> I.ScheduleNode:
        return self.node.band_scale(self.get_tiling_sizes(sizes))

    @transformation
    def scale_down(self, sizes: int | List[int]) -> I.ScheduleNode:
        return self.node.band_scale_down(self.get_tiling_sizes(sizes))

    @transformation
    def mod(self, sizes: int | List[int]) -> I.ScheduleNode:
        return self.node.band_mod(self.get_tiling_sizes(sizes))

    @transformation
    def shift(self, sizes: int | List[int]) -> I.ScheduleNode:
        depth = self.node.band_get_space().dim(3)
        sizes = [sizes] * depth if isinstance(sizes, int) else sizes
        if not len(sizes) == depth:
            raise ValueError(
                f"Shift size mismatch: Band depth is {depth}, "
                f"but provided {len(sizes)} sizes: {sizes}."
            )
        mv = I.MultiVal.zero(self.node.band_get_space())
        for i, size in enumerate(sizes):
            mv = mv.set_val(i, I.Val.int_from_si(int(size)))
        domain = self.node.get_subtree_expansion().domain()
        shift = I.MultiUnionPwAff.multi_val_on_domain(domain, mv)
        return self.node.band_shift(shift)

    @transformation
    def tile(self, sizes: int | List[int]) -> I.ScheduleNode:
        return self.node.band_tile(self.get_tiling_sizes(sizes))

    @transformation
    def split(self, pos: int) -> I.ScheduleNode:
        return self.node.band_split(pos)

    @transformation
    def sink(self) -> I.ScheduleNode:
        return self.node.band_sink()

    @transformation
    def permute(self, *order: List[int]) -> I.ScheduleNode:
        if sorted(order) != list(range(self.depth)):
            raise RuntimeError(f"order is not a valid permutation, getting {order_arg}, depth={self.depth}")
        def _perm(lst): return [lst[i] for i in order]
        
        mupa = self.node.band_get_partial_schedule()
        
        upas = _perm([mupa.get_union_pw_aff(i) for i in range(self.depth)])
        coincidents = _perm([self.node.band_member_get_coincident(i) for i in range(self.depth)])
        for i in range(self.depth):
            mupa = mupa.set_union_pw_aff(i, upas[i])
        band = self.node.insert_partial_schedule(mupa)
        for i in range(self.depth):
            band = self.node.band_member_set_coincident(i, coincidents[i])
        return band

    def __mul__(self, other: Any) -> "BandEditor": return self.scale(other)
    def __floordiv__(self, other: Any) -> "BandEditor": return self.scale_down(other)
    def __mod__(self, other: Any) -> "BandEditor": return self.mod(other)
    def __add__(self, other: Any) -> "BandEditor": return self.shift(other)
    def __sub__(self, other: Any) -> "BandEditor": return self.shift([-x for x in other])
    def __matmul__(self, other: Any) -> "BandEditor": return self.tile(other)

class SequenceEditor(Dispatcher):
    @transformation
    def fuse(self) -> I.ScheduleNode:
        return schedule_node_sequence_full_fuse(self.node)

class SetEditor(Dispatcher):
    @transformation
    def fuse(self) -> I.ScheduleNode:
        return schedule_node_sequence_full_fuse(self.node)
