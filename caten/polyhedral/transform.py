from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Callable, Dict, List, Optional, Sequence, Tuple
import functools
import re

import caten.isl as I
from .analysis import compute_dependence_relation, schedule_is_legal_p, schedule_node_band_parallel_legal_p
# ============================================================================================================
@dataclass(frozen=True)
class TransformRecord:
    name: str
    args: Tuple[Any, ...]
    kwargs: Dict[str, Any]
    path: Tuple[int, ...]
    new_path: Tuple[int, ...]

@dataclass
class TraceNode:
    ops: List[TransformRecord] = field(default_factory=list)
    children: Dict[int, "TraceNode"] = field(default_factory=dict)

class OptimizationTrace:
    def __init__(self) -> None:
        self.root = TraceNode()

    def record(self, record: TransformRecord) -> None:
        node = self.root
        for idx in record.path:
            node = node.children.setdefault(idx, TraceNode())
        node.ops.append(record)

    def _format_value(self, value: Any) -> str:
        if isinstance(value, str):
            return repr(value)
        if isinstance(value, (int, float, bool, type(None))):
            return repr(value)
        if isinstance(value, (list, tuple)):
            items = ", ".join(self._format_value(v) for v in value)
            wrap = "[{}]" if isinstance(value, list) else "({})"
            return wrap.format(items)
        if hasattr(value, "__str__") and value.__class__.__module__.startswith("caten.isl"):
            return f"I.{value.__class__.__name__}({str(value)!r})"
        return repr(value)

    def to_python(self, root: str = "root") -> str:
        lines: List[str] = []

        def emit(node: TraceNode, path: List[int]) -> None:
            for record in node.ops:
                path_expr = "".join(f".child({i})" for i in path)
                args = ", ".join(self._format_value(a) for a in record.args)
                kwargs = ", ".join(f"{k}={self._format_value(v)}" for k, v in record.kwargs.items())
                joined = ", ".join(x for x in (args, kwargs) if x)
                lines.append(f"{root}{path_expr}.{record.name}({joined})")
            for idx, child in sorted(node.children.items()):
                emit(child, path + [idx])

        emit(self.root, [])
        return "\n".join(lines)

    def to_tree(self) -> str:
        lines: List[str] = []

        def render(node: TraceNode, prefix: str) -> None:
            for record in node.ops:
                args = ", ".join(self._format_value(a) for a in record.args)
                kwargs = ", ".join(f"{k}={self._format_value(v)}" for k, v in record.kwargs.items())
                joined = ", ".join(x for x in (args, kwargs) if x)
                lines.append(f"{prefix}{record.name}({joined})")
            for idx, child in sorted(node.children.items()):
                lines.append(f"{prefix}child({idx})")
                render(child, prefix + "  ")

        render(self.root, "")
        return "\n".join(lines)
# ============================================================================================================
def transform_opt(fn: Callable[..., Any]) -> Callable[..., Any]:
    @functools.wraps(fn)
    def wrapper(self: "Dispatcher", *args: Any, **kwargs: Any) -> Any:
        before_path = self._path
        node = fn(self, *args, **kwargs)
        if not hasattr(node, "get_schedule"):
            return node
        return self._commit(node, fn.__name__, args, kwargs, before_path)
    return wrapper
# ============================================================================================================
def _node_path(node: "I.ScheduleNode") -> Tuple[int, ...]:
    path: List[int] = []
    current = node
    while current.has_parent():
        path.append(current.get_child_position())
        current = current.parent()
    return tuple(reversed(path))

def schedule_node_at_path(node: "I.ScheduleNode", path: Sequence[int]) -> "I.ScheduleNode":
    current = node
    for idx in path:
        current = current.child(idx)
    return current

def schedule_gather_path(
    node: "I.ScheduleNode", predicate: Callable[["I.ScheduleNode", Sequence[int]], bool]
) -> List[List[int]]:
    results: List[List[int]] = []

    def explore(n: "I.ScheduleNode", path: List[int]) -> None:
        if predicate(n, path):
            results.append(list(path))
        for i in range(n.n_children()):
            explore(n.child(i), path + [i])

    explore(node, [])
    return results


def schedule_get_non_marked_sequence_set(schedule: I.Schedule) -> List[I.ScheduleNode]:
    root = schedule.get_root()
    paths = schedule_gather_path(
        root,
        lambda n, _p: n.get_type_name() in ("sequence", "set")
        and n.n_children() > 1
        and n.parent().get_type_name() != "mark",
    )
    return [schedule_node_at_path(root, path) for path in paths]


def schedule_remove_all_marks(schedule: I.Schedule) -> I.Schedule:
    root = schedule.get_root()
    while True:
        paths = schedule_gather_path(root, lambda n, _p: n.get_type_name() == "mark")
        if not paths:
            return schedule
        node = schedule_node_at_path(root, paths[0])
        schedule = node.delete().get_schedule()
        root = schedule.get_root()


def schedule_remove_empty_schedule(schedule: I.Schedule) -> I.Schedule:
    root = schedule.get_root()
    paths = schedule_gather_path(
        root,
        lambda n, _p: (n.get_type_name() == "band" and n.band_get_partial_schedule().size() == 0)
        or n.get_type_name() == "leaf",
    )
    for path in paths:
        node = schedule_node_at_path(root, path)
        schedule = node.delete().get_schedule()
        root = schedule.get_root()
    return schedule


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
        self._deps: Optional[I.UnionMap] = None
        self.trace = OptimizationTrace()

    @classmethod
    def from_schedule(
        cls,
        schedule: I.Schedule,
        read_umap: Optional[I.UnionMap],
        write_umap: Optional[I.UnionMap],
        stmts: Dict[str, Callable],
    ) -> "ConstraintedModel":
        return cls(schedule, read_umap, write_umap, stmts)

    def _ensure_deps(self) -> Optional[I.UnionMap]:
        if self._deps is not None:
            return self._deps
        if self.read_umap is None or self.write_umap is None:
            return None
        deps, *_ = compute_dependence_relation(self.read_umap, self.write_umap, self.schedule)
        self._deps = deps
        return self._deps

    def editor(self) -> "Dispatcher":
        return Dispatcher(self.schedule.get_root(), self)

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

    def __lshift__(self, directive: Any) -> "ConstraintedModel":
        from caten.polyhedral.codegen import Directive
        if not isinstance(directive, Directive):
            raise TypeError("ConstraintedModel << expects a Directive instance.")
        root = self.schedule.get_root()
        new_node = directive.on_schedule(root)
        new_schedule = new_node.get_schedule()
        deps = self._ensure_deps()
        if deps is not None and not schedule_is_legal_p(new_schedule, deps):
            raise RuntimeError("Illegal schedule after directive application.")
        self.schedule = new_schedule
        self._deps = None
        return self


class Dispatcher:
    def __init__(
        self,
        schedule: I.ScheduleNode,
        model: ConstraintedModel,
        path: Optional[Sequence[int]] = None,
    ) -> None:
        self.model = model
        self._path = tuple(path) if path is not None else _node_path(schedule)

    @property
    def current(self) -> I.ScheduleNode:
        return schedule_node_at_path(self.model.schedule.get_root(), self._path)

    def _commit(
        self,
        new_node: I.ScheduleNode,
        name: str,
        args: Tuple[Any, ...],
        kwargs: Dict[str, Any],
        before_path: Tuple[int, ...],
    ) -> "Dispatcher":
        new_schedule = new_node.get_schedule()
        deps = self.model._ensure_deps()
        if deps is not None and not schedule_is_legal_p(new_schedule, deps):
            raise RuntimeError(f"Illegal schedule after {name} at path {before_path}.")
        self.model.schedule = new_schedule
        new_path = _node_path(new_node)
        self.model.trace.record(TransformRecord(name, args, kwargs, before_path, new_path))
        self._path = new_path
        return self

    def to_c(self) -> str:
        from caten.polyhedral.codegen import schedule_to_c

        return schedule_to_c(self.current.get_schedule(), self.model.stmts)

    def __repr__(self) -> str:
        from caten.polyhedral.viz import print_schedule

        return f"{self.__class__.__name__}(\n{print_schedule(self.current)}\n)"

    def __enter__(self) -> "Dispatcher":
        return self

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        return None

    def ensure_and_dispatch(self, cls: type["Dispatcher"], expect: str) -> "Dispatcher":
        if not self.current.get_type_name() == expect:
            raise RuntimeError(
                f"Cannot switch to the {expect}, you are now at {self.current.get_type_name()}!.\n"
                f"{self.__repr__()}"
            )
        return cls(self.current, self.model, self._path)

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
        return Dispatcher(self.current.child(key), self.model)

    @transform_opt
    def mark(self, directive: Any) -> I.ScheduleNode:
        from caten.polyhedral.codegen import Directive

        if isinstance(directive, Directive):
            return self.current.insert_mark(directive.mark_id())
        if isinstance(directive, str):
            return self.current.insert_mark(I.Id.alloc(directive))
        raise TypeError("mark expects a Directive or string id.")

class DomainEditor(Dispatcher):
    def padding(self) -> "DomainEditor":
        return self

    def reshape(self) -> "DomainEditor":
        return self

class FilterEditor(Dispatcher):
    pass

class BandEditor(Dispatcher):
    def get_tiling_sizes(self, sizes: int | List[int]) -> I.MultiVal:
        depth = self.current.band_get_space().dim(3)
        sizes = [sizes] * depth if isinstance(sizes, int) else sizes
        if not len(sizes) == depth:
            raise ValueError(
                f"Tiling size mismatch: Band depth is {depth}, "
                f"but provided {len(sizes)} sizes: {sizes}."
            )
        mv = I.MultiVal.zero(self.current.band_get_space())
        for i, size in enumerate(sizes):
            mv = mv.set_val(i, I.Val.int_from_si(int(size)))
        return mv

    @property
    def depth(self) -> int:
        return self.current.band_get_space().dim(3)

    @transform_opt
    def scale(self, sizes: int | List[int]) -> I.ScheduleNode:
        return self.current.band_scale(self.get_tiling_sizes(sizes))

    @transform_opt
    def scale_down(self, sizes: int | List[int]) -> I.ScheduleNode:
        return self.current.band_scale_down(self.get_tiling_sizes(sizes))

    @transform_opt
    def mod(self, sizes: int | List[int]) -> I.ScheduleNode:
        return self.current.band_mod(self.get_tiling_sizes(sizes))

    @transform_opt
    def shift(self, sizes: int | List[int]) -> I.ScheduleNode:
        depth = self.current.band_get_space().dim(3)
        sizes = [sizes] * depth if isinstance(sizes, int) else sizes
        if not len(sizes) == depth:
            raise ValueError(
                f"Shift size mismatch: Band depth is {depth}, "
                f"but provided {len(sizes)} sizes: {sizes}."
            )
        mv = I.MultiVal.zero(self.current.band_get_space())
        for i, size in enumerate(sizes):
            mv = mv.set_val(i, I.Val.int_from_si(int(size)))
        domain = schedule_node_subtree_domain(self.current)
        shift = I.MultiUnionPwAff.multi_val_on_domain(domain, mv)
        return self.current.band_shift(shift)

    @transform_opt
    def tile(self, sizes: int | List[int]) -> I.ScheduleNode:
        return self.current.band_tile(self.get_tiling_sizes(sizes))

    @transform_opt
    def split(self, pos: int) -> I.ScheduleNode:
        return self.current.band_split(pos)

    @transform_opt
    def sink(self) -> I.ScheduleNode:
        return self.current.band_sink()

    @transform_opt
    def permute(self, order: List[int]) -> I.ScheduleNode:
        return schedule_node_band_permute(self.current, order)

    def __mul__(self, other: Any) -> "BandEditor":
        return self.scale(other)

    def __floordiv__(self, other: Any) -> "BandEditor":
        return self.scale_down(other)

    def __mod__(self, other: Any) -> "BandEditor":
        return self.mod(other)

    def __add__(self, other: Any) -> "BandEditor":
        return self.shift(other)

    def __sub__(self, other: Any) -> "BandEditor":
        if isinstance(other, list):
            return self.shift([-x for x in other])
        return self.shift(-other)

    def __matmul__(self, other: Any) -> "BandEditor":
        return self.tile(other)

class SequenceEditor(Dispatcher):
    @transform_opt
    def fuse(self) -> I.ScheduleNode:
        return schedule_node_sequence_full_fuse(self.current)

class SetEditor(Dispatcher):
    @transform_opt
    def fuse(self) -> I.ScheduleNode:
        return schedule_node_sequence_full_fuse(self.current)
