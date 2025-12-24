from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Callable, Dict, List, Optional, Sequence, Tuple
import functools
import re

import caten.isl as I
from .analysis import compute_dependence_relation, schedule_is_legal_p, schedule_node_band_parallel_legal_p


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


def transform_opt(fn: Callable[..., Any]) -> Callable[..., Any]:
    @functools.wraps(fn)
    def wrapper(self: "Dispatcher", *args: Any, **kwargs: Any) -> Any:
        before_path = self._path
        node = fn(self, *args, **kwargs)
        if not hasattr(node, "get_schedule"):
            return node
        return self._commit(node, fn.__name__, args, kwargs, before_path)

    return wrapper


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
        if self.read_umap is None:
            read_umap = other.read_umap
        elif other.read_umap is None:
            read_umap = self.read_umap
        else:
            read_umap = self.read_umap | other.read_umap
        if self.write_umap is None:
            write_umap = other.write_umap
        elif other.write_umap is None:
            write_umap = self.write_umap
        else:
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

    @transform_opt
    def reorder(self, order: List[int]) -> I.ScheduleNode:
        return schedule_node_sequence_reorder(self.current, order)

    def tpsort(self, read_umap: I.UnionMap, write_umap: I.UnionMap) -> List[int]:
        return schedule_node_sequence_tpsort(self.current, read_umap, write_umap)

    @transform_opt
    def group_sequence(self) -> I.ScheduleNode:
        return schedule_node_sequence_group_sequence(self.current)


class SetEditor(Dispatcher):
    @transform_opt
    def fuse(self) -> I.ScheduleNode:
        return schedule_node_sequence_full_fuse(self.current)


# ------------------------------
# Schedule-node transformations
# ------------------------------


def permutations(lst: List[Any]) -> List[List[Any]]:
    if not lst:
        return [[]]
    results: List[List[Any]] = []
    for i, x in enumerate(lst):
        rest = lst[:i] + lst[i + 1 :]
        for y in permutations(rest):
            results.append([x] + y)
    return results


def permute_list(order: List[int], items: List[Any]) -> List[Any]:
    if len(order) != len(items):
        raise ValueError("permute_list: order and items must have equal length.")
    if sorted(order) != list(range(len(items))):
        raise ValueError("permute_list: order must be a permutation of indices.")
    return [items[i] for i in order]


def schedule_node_band_get_depth(band: I.ScheduleNode) -> int:
    return band.band_get_space().dim(3)


def schedule_node_band_get_coincident(band: I.ScheduleNode) -> List[int]:
    depth = schedule_node_band_get_depth(band)
    return [1 if band.band_member_get_coincident(i) else 0 for i in range(depth)]


def schedule_node_band_permute(band: I.ScheduleNode, order: List[int]) -> I.ScheduleNode:
    if not band.band_get_permutable():
        raise ValueError("schedule_node_band_permute: band must be permutable.")
    depth = schedule_node_band_get_depth(band)
    if depth <= 0:
        raise ValueError("schedule_node_band_permute: band depth must be positive.")
    if len(order) != depth:
        raise ValueError("schedule_node_band_permute: order size must match band depth.")
    if sorted(order) != list(range(depth)):
        raise ValueError("schedule_node_band_permute: order must be a permutation.")
    mupa = band.band_get_partial_schedule()
    if mupa.size() != depth:
        raise ValueError("schedule_node_band_permute: partial schedule size mismatch.")
    coincidents = schedule_node_band_get_coincident(band)
    upas = [mupa.get_union_pw_aff(i) for i in range(depth)]
    coincidents_new = permute_list(order, coincidents)
    upas_new = permute_list(order, upas)
    for i, upa in enumerate(upas_new):
        mupa = mupa.set_union_pw_aff(i, upa)
    band = band.insert_partial_schedule(mupa)
    for i, coincident in enumerate(coincidents_new):
        band = band.band_member_set_coincident(i, coincident)
    return band


def _as_val(value: int | I.Val) -> I.Val:
    if isinstance(value, I.Val):
        return value
    return I.Val.int_from_si(int(value))


def _val_min(values: Sequence[I.Val]) -> I.Val:
    result = values[0]
    for v in values[1:]:
        result = result.min(v)
    return result


def _val_max(values: Sequence[I.Val]) -> I.Val:
    result = values[0]
    for v in values[1:]:
        result = result.max(v)
    return result


def tiling_size(band: I.ScheduleNode, size: int | I.Val) -> I.MultiVal:
    band_space = band.band_get_space()
    depth = band_space.dim(3)
    mv = I.MultiVal.zero(band_space)
    val = _as_val(size)
    for i in range(depth):
        mv = mv.set_val(i, val)
    return mv


def _multival_from_sizes(band: I.ScheduleNode, sizes: Sequence[int]) -> I.MultiVal:
    depth = band.band_get_space().dim(3)
    if len(sizes) != depth:
        raise ValueError(
            f"Size mismatch: band depth is {depth}, but got {len(sizes)} sizes."
        )
    mv = I.MultiVal.zero(band.band_get_space())
    for i, size in enumerate(sizes):
        mv = mv.set_val(i, I.Val.int_from_si(int(size)))
    return mv


def union_set_add_tiling_isolation_constraint(
    dom: I.UnionSet,
    width: I.Val,
    dim_name: str,
    dom_size: I.Val,
    is_full_tile: bool,
) -> I.UnionSet:
    lst = dom.get_set_list()
    cnt = lst.n_set()
    res: Optional[I.UnionSet] = None
    bound = dom_size.sub(dom_size.mod(width)).sub(I.Val.int_from_si(1))
    if not is_full_tile:
        bound = bound.add(I.Val.int_from_si(1))
    for i in range(cnt):
        s = lst.get_at(i)
        nd = s.dim(3)
        pos = None
        for k in range(nd):
            nm = s.get_dim_name(3, k)
            if nm and nm == dim_name:
                pos = k
                break
        if pos is not None:
            sp = s.get_space()
            ls = I.LocalSpace.from_space(sp)
            ineq = I.Constraint.inequality(ls)
            if is_full_tile:
                ineq = ineq.set_constant_val(bound)
                ineq = ineq.set_coefficient_si(3, pos, -1)
            else:
                ineq = ineq.set_constant_val(bound.neg())
                ineq = ineq.set_coefficient_si(3, pos, 1)
            s = s.add_constraint(ineq)
            u = I.UnionSet.from_set(s)
            res = u if res is None else res.union(u)
        else:
            u = I.UnionSet.from_set(s)
            res = u if res is None else res.union(u)
    return res if res is not None else I.UnionSet.empty(dom.get_space())


def union_set_list_add_nonempty(lst: I.UnionSetList, uset: Optional[I.UnionSet]) -> I.UnionSetList:
    if uset is None or uset.is_empty():
        return lst
    return lst.add(uset)


def domain_dimension_maxima_from_union_map(umap: I.UnionMap) -> Dict[str, I.Val]:
    domain = umap.domain()
    sets = domain.get_set_list()
    n = sets.n_set()
    maxima: Dict[str, I.Val] = {}
    for i in range(n):
        s = sets.get_at(i)
        nd = s.dim(3)
        for d in range(nd):
            name = s.get_dim_name(3, d)
            if not name:
                continue
            vmax = s.dim_max_val(d)
            if name in maxima:
                maxima[name] = maxima[name].max(vmax)
            else:
                maxima[name] = vmax
    return maxima


def partial_schedule_get_involved_dims(mupa: I.MultiUnionPwAff) -> List[str]:
    text = str(mupa)
    dims: List[str] = []
    for _name, dom_expr, rhs in re.findall(
        r"([A-Za-z_][\w]*)\[([^\]]+)\]\s*->\s*\[([^\]]+)\]", text
    ):
        dom_vars = [v.strip() for v in dom_expr.split(",")]
        for var in dom_vars:
            if not var:
                continue
            if re.search(rf"\b{re.escape(var)}\b", rhs) and var not in dims:
                dims.append(var)
    return dims


def union_set_filter_by_dim_name(uset: I.UnionSet, dim_name: str) -> I.UnionSet:
    lst = uset.get_set_list()
    n = lst.n_set()
    acc: Optional[I.UnionSet] = None
    for i in range(n):
        s = lst.get_at(i)
        nd = s.dim(3)
        hit = False
        for k in range(nd):
            nm = s.get_dim_name(3, k)
            if nm and nm == dim_name:
                hit = True
                break
        if hit:
            acc = I.UnionSet.from_set(s) if acc is None else acc.union(I.UnionSet.from_set(s))
    return acc if acc is not None else I.UnionSet.empty(uset.get_space())


def make_full_partial_filters(
    subdom: I.UnionSet,
    tiled_ids: List[str],
    maxima: Dict[str, I.Val],
    width: I.Val,
) -> Tuple[I.UnionSet, I.UnionSet]:
    full = subdom
    tail = subdom
    for name in tiled_ids:
        max_val = maxima[name]
        full = union_set_add_tiling_isolation_constraint(full, width, name, max_val, True)
        tail = union_set_add_tiling_isolation_constraint(tail, width, name, max_val, False)
    return full, tail


def schedule_node_band_tile_star(
    band: I.ScheduleNode,
    size: int | I.Val,
    strategy: str = "atomic",
    scale: Optional[int | I.Val] = None,
) -> I.ScheduleNode:
    tiled = band.band_tile(tiling_size(band, size))
    if scale is not None:
        tiled = tiled.band_scale(tiling_size(band, scale))
    if strategy != "isolate":
        return tiled
    subdom = tiled.get_subtree_expansion().domain()
    mupa = tiled.band_get_partial_schedule()
    tiled_ids = partial_schedule_get_involved_dims(mupa)
    if not tiled_ids:
        return tiled
    maxima = domain_dimension_maxima_from_union_map(tiled.get_prefix_schedule_relation())
    width = _as_val(size)
    affected = I.UnionSet.from_str("{}")
    for nm in tiled_ids:
        affected = affected.union(union_set_filter_by_dim_name(subdom, nm))
    unaffected = subdom.subtract(affected)
    full, tail = make_full_partial_filters(affected, tiled_ids, maxima, width)
    uset_list = I.UnionSetList.alloc(0)
    uset_list = union_set_list_add_nonempty(uset_list, unaffected)
    uset_list = union_set_list_add_nonempty(uset_list, full)
    uset_list = union_set_list_add_nonempty(uset_list, tail)
    return tiled.insert_sequence(uset_list)


def schedule_node_band_separate(band: I.ScheduleNode, size: int | I.Val) -> I.ScheduleNode:
    mv = tiling_size(band, size)
    tiled = band.band_tile(mv)
    return tiled.band_scale_down(mv)


def schedule_node_count_bands(node: I.ScheduleNode) -> int:
    count = schedule_node_band_get_depth(node) if node.get_type_name() == "band" else 0
    for i in range(node.n_children()):
        count += schedule_node_count_bands(node.child(i))
    return count


def schedule_get_roots(schedule: I.Schedule) -> List[I.ScheduleNode]:
    root = schedule.get_root().child(0)
    if root.get_type_name() == "sequence":
        return [root.child(i) for i in range(root.n_children())]
    return [root]


def schedule_node_subtree_domain(node: I.ScheduleNode) -> I.UnionSet:
    return node.get_subtree_expansion().domain()


def schedule_node_sequence_check_fusible(node: I.ScheduleNode) -> str:
    if node.get_type_name() not in ("sequence", "set"):
        raise ValueError("schedule_node_sequence_check_fusible expects sequence/set node.")
    mupa: Optional[I.MultiUnionPwAff] = None
    for i in range(node.n_children()):
        filter_node = node.child(i)
        filter_set = filter_node.filter_get_filter()
        band_node = filter_node.first_child()
        if band_node.get_type_name() != "band":
            return "need_reorder"
        tmp = band_node.band_get_partial_schedule()
        tmp = tmp.intersect_domain(filter_set)
        tmp = tmp.reset_tuple_id(3)
        mupa = tmp if mupa is None else mupa.union_add(tmp)
    return "valid"


def schedule_node_sequence_full_fuse(node: I.ScheduleNode) -> I.ScheduleNode:
    if node.get_type_name() not in ("sequence", "set"):
        raise ValueError("schedule_node_sequence_full_fuse expects sequence/set node.")
    mark = I.Id.alloc("@ApplyOptimization{FULL_FUSE}")
    node = node.insert_mark(mark).first_child()
    n_child = node.n_children()
    cursor = node.first_child()
    mupa: Optional[I.MultiUnionPwAff] = None
    for i in range(n_child):
        filter_set = cursor.filter_get_filter()
        band_node = cursor.first_child()
        if band_node.get_type_name() != "band":
            raise ValueError("schedule_node_sequence_full_fuse: filter child must be a band.")
        tmp = band_node.band_get_partial_schedule()
        tmp = tmp.intersect_domain(filter_set)
        tmp = tmp.reset_tuple_id(3)
        mupa = tmp if mupa is None else mupa.union_add(tmp)
        cursor = band_node.delete().parent()
        if i == n_child - 1:
            cursor = cursor.parent()
        else:
            cursor = cursor.next_sibling()
    if mupa is not None:
        cursor = cursor.insert_partial_schedule(mupa)
    return cursor


def band_range_union_set(band_node: I.ScheduleNode, user_domain: I.UnionSet) -> I.UnionSet:
    prefix = band_node.get_prefix_schedule_union_map()
    part = band_node.band_get_partial_schedule_union_map()
    full = prefix.flat_range_product(part)
    full = full.intersect_domain(user_domain)
    return full.range()


def uset_max_min(uset: I.UnionSet) -> Tuple[I.Val, I.Val]:
    sets = uset.get_set_list()
    if sets.n_set() != 1:
        raise ValueError("uset_max_min expects a union set with a single set.")
    s = sets.get_at(0)
    dim = s.dim(3)
    vmax = s.dim_max_val(dim - 1)
    vmin = s.dim_min_val(dim - 1)
    return vmax, vmin


def schedule_node_sequence_get_band_sizes(
    domain: I.UnionSet, components: I.ScheduleNode
) -> Tuple[List[I.Val], I.Val, I.Val, bool, bool]:
    if components.get_type_name() not in ("sequence", "set"):
        raise ValueError("schedule_node_sequence_get_band_sizes expects sequence/set node.")
    sizes: List[I.Val] = []
    for i in range(components.n_children()):
        band_node = components.child(i).first_child()
        if band_node.get_type_name() != "band":
            raise ValueError(
                "schedule_node_sequence_get_band_sizes: children must have band nodes."
            )
        uset = band_range_union_set(band_node, domain)
        vmax, vmin = uset_max_min(uset)
        sizes.append(vmax.add(I.Val.int_from_si(1)))
    min_band = _val_min(sizes)
    max_band = _val_max(sizes)
    is_same = min_band.eq(max_band)
    is_multiple = all(size.mod(min_band).is_zero() for size in sizes)
    return sizes, min_band, max_band, is_same, is_multiple


def schedule_node_sequence_align_band_size(
    components: I.ScheduleNode, sizes: List[I.Val]
) -> I.ScheduleNode:
    if components.get_type_name() not in ("sequence", "set"):
        raise ValueError("schedule_node_sequence_align_band_size expects sequence/set node.")
    min_band = _val_min(sizes)
    max_band = _val_max(sizes)
    if min_band.eq(max_band):
        return components
    node = components
    for i in range(node.n_children()):
        size = sizes[i]
        if not size.eq(max_band):
            node = node.child(i).first_child()
            if node.get_type_name() != "band":
                raise ValueError(
                    "schedule_node_sequence_align_band_size: children must have band nodes."
                )
            node = schedule_node_band_separate(node, max_band.div(size))
            node = node.parent().parent()
    return node


def schedule_node_band_reshape(band: I.ScheduleNode, band_size: I.Val, reshape_to: I.Val) -> I.ScheduleNode:
    size = band_size.div(reshape_to).floor()
    return schedule_node_band_tile_star(band, size, strategy="atomic", scale=reshape_to.div(band_size))


def schedule_node_sequence_apply_flash(
    domain: I.UnionSet, components: I.ScheduleNode, domain_size: I.Val
) -> I.ScheduleNode:
    if components.get_type_name() not in ("sequence", "set"):
        raise ValueError("schedule_node_sequence_apply_flash expects sequence/set node.")
    node = components
    for i in range(node.n_children()):
        node = node.child(i).first_child()
        if node.get_type_name() != "band":
            raise ValueError(
                "schedule_node_sequence_apply_flash: children must have band nodes."
            )
        uset = band_range_union_set(node, domain)
        vmax, vmin = uset_max_min(uset)
        if not vmin.is_zero():
            raise ValueError("schedule_node_sequence_apply_flash expects min=0 domain.")
        vmax = vmax.add(I.Val.int_from_si(1))
        if not vmax.eq(domain_size):
            node = schedule_node_band_chain_sink(
                schedule_node_band_reshape(node, vmax, domain_size).first_child()
            ).parent()
        node = node.parent().parent()
    return node


def schedule_node_band_get_n_chain(band: I.ScheduleNode) -> Tuple[int, I.ScheduleNode]:
    depth = 0
    last_band = band
    node = band.first_child()
    while node.get_type_name() == "band":
        depth += 1
        last_band = node
        node = node.first_child()
    return depth, last_band


def schedule_node_band_scoop_up(band: I.ScheduleNode, n: int) -> I.ScheduleNode:
    depth, innermost_band = schedule_node_band_get_n_chain(band)
    if n <= 0 or n > depth:
        raise ValueError("schedule_node_band_scoop_up: n is out of bound.")
    for _ in range(depth, n, -1):
        innermost_band = innermost_band.parent()
    mupa = innermost_band.band_get_partial_schedule()
    node = innermost_band.delete()
    for _ in range(n):
        node = node.parent()
    return node.insert_partial_schedule(mupa)


def schedule_node_band_chain_sink(band: I.ScheduleNode) -> I.ScheduleNode:
    if band.first_child().get_type_name() != "band":
        return band
    mupa = band.band_get_partial_schedule()
    depth, innermost_band = schedule_node_band_get_n_chain(band.delete())
    top = innermost_band.first_child().insert_partial_schedule(mupa)
    for _ in range(depth + 1):
        top = top.parent()
    return top


def schedule_node_sequence_splice_children(node: I.ScheduleNode) -> I.ScheduleNode:
    if node.get_type_name() != "sequence":
        raise ValueError("schedule_node_sequence_splice_children expects sequence node.")
    for i in range(node.n_children()):
        child = node.child(i)
        if child.child(0).get_type_name() == "sequence":
            return schedule_node_sequence_splice_children(node.sequence_splice_child(i))
    return node


def schedule_node_sequence_get_filters(components: I.ScheduleNode) -> List[I.UnionSet]:
    if components.get_type_name() != "sequence":
        raise ValueError("schedule_node_sequence_get_filters expects sequence node.")
    return [components.child(i).filter_get_filter() for i in range(components.n_children())]


def schedule_node_sequence_get_filter_types(components: I.ScheduleNode) -> List[str]:
    if components.get_type_name() != "sequence":
        raise ValueError("schedule_node_sequence_get_filter_types expects sequence node.")
    return [components.child(i).first_child().get_type_name() for i in range(components.n_children())]


def schedule_node_sequence_reorder(components: I.ScheduleNode, order: List[int]) -> I.ScheduleNode:
    if components.get_type_name() != "sequence":
        raise ValueError("schedule_node_sequence_reorder expects sequence node.")
    new_filters = permute_list(order, schedule_node_sequence_get_filters(components))
    new_filter_list = I.UnionSetList.alloc(0)
    for f in new_filters:
        new_filter_list = new_filter_list.add(f)
    mark = I.Id.alloc(f"@ApplyOptimization{{REORDER}}{order}")
    return components.insert_sequence(new_filter_list).insert_mark(mark).first_child()


def _collect_umap_pairs(umap: I.UnionMap) -> List[Tuple[str, str]]:
    pairs: List[Tuple[str, str]] = []
    for match in re.findall(r"([A-Za-z_][\w]*)\[[^\]]*\]\s*->\s*([A-Za-z_][\w]*)\[", str(umap)):
        pairs.append(match)
    return pairs


def _umap_collect_range_names(umap: I.UnionMap, domain_name: Optional[str] = None) -> List[str]:
    names: List[str] = []
    for dom, ran in _collect_umap_pairs(umap):
        if domain_name is None or dom == domain_name:
            names.append(ran)
    return list(dict.fromkeys(names))


def union_set_single_tuple_name(uset: I.UnionSet) -> str:
    sl = uset.get_set_list()
    if sl.n_set() != 1:
        raise ValueError("union_set_single_tuple_name expects a single set.")
    s = sl.get_at(0)
    name = s.get_tuple_name()
    if name is None:
        raise ValueError("union_set_single_tuple_name: set has no name.")
    return name


def schedule_node_sequence_tpsort(
    components: I.ScheduleNode, read_umap: I.UnionMap, write_umap: I.UnionMap
) -> List[int]:
    if components.get_type_name() != "sequence":
        raise ValueError("schedule_node_sequence_tpsort expects sequence node.")
    filters = schedule_node_sequence_get_filters(components)
    filter_types = schedule_node_sequence_get_filter_types(components)
    filter_ids = list(range(len(filters)))
    written_buffs = _umap_collect_range_names(write_umap, None)
    filter_is_load: List[bool] = []
    for f, ft in zip(filters, filter_types):
        if ft == "leaf":
            name = union_set_single_tuple_name(f)
            read_buffs = _umap_collect_range_names(read_umap, name)
            is_load = not read_buffs or all(b not in written_buffs for b in read_buffs)
            filter_is_load.append(is_load)
        else:
            filter_is_load.append(False)
    new_orders: List[int] = []
    for flag, idx in zip(filter_is_load, filter_ids):
        if flag:
            new_orders.append(idx)
    for flag, idx in zip(filter_is_load, filter_ids):
        if not flag:
            new_orders.append(idx)
    return new_orders


def schedule_node_sequence_group_sequence(components: I.ScheduleNode) -> I.ScheduleNode:
    if components.get_type_name() != "sequence":
        raise ValueError("schedule_node_sequence_group_sequence expects sequence node.")
    filters = schedule_node_sequence_get_filters(components)
    filter_types = [typ == "band" for typ in schedule_node_sequence_get_filter_types(components)]
    last_filter: Optional[I.UnionSet] = None
    new_filter_list = I.UnionSetList.alloc(0)
    for flt, is_band in zip(filters, filter_types):
        if is_band:
            last_filter = flt if last_filter is None else last_filter.union(flt)
        else:
            if last_filter is not None:
                new_filter_list = new_filter_list.add(last_filter)
                last_filter = None
            new_filter_list = new_filter_list.add(flt)
    if last_filter is not None:
        new_filter_list = new_filter_list.add(last_filter)
    return components.insert_sequence(new_filter_list)


def schedule_compute_parallel(schedule: I.Schedule, deps: I.UnionMap) -> I.Schedule:
    root = schedule.get_root()
    paths = schedule_gather_path(root, lambda n, _p: n.get_type_name() == "band")
    for path in paths:
        node = schedule_node_at_path(root, path)
        if node.band_n_member() != 1:
            continue
        coincident = 1 if schedule_node_band_parallel_legal_p(node, deps) else 0
        node = node.band_member_set_coincident(0, coincident)
        schedule = node.get_schedule()
        root = schedule.get_root()
    return schedule


def schedule_split_all_band(schedule: I.Schedule) -> I.Schedule:
    root = schedule.get_root()
    paths = schedule_gather_path(root, lambda n, _p: n.get_type_name() == "band")
    for path in sorted(paths, key=len, reverse=True):
        node = schedule_node_at_path(root, path)
        depth = schedule_node_band_get_depth(node)
        if depth <= 1:
            continue
        outer = node.band_split(1)
        inner = outer.child(0)
        for _ in range(depth - 2):
            inner = inner.band_split(1).child(0)
        schedule = outer.get_schedule()
        root = schedule.get_root()
    return schedule


def schedule_fuse_all_band(schedule: I.Schedule) -> I.Schedule:
    root = schedule.get_root()
    paths = schedule_gather_path(root, lambda n, _p: n.get_type_name() == "band")
    for path in sorted(paths, key=len, reverse=True):
        node = schedule_node_at_path(root, path)
        depth, _ = schedule_node_band_get_n_chain(node)
        if depth == 0:
            continue
        mupa = node.band_get_partial_schedule()
        coincidents: List[int] = [
            1 if node.band_member_get_coincident(i) else 0 for i in range(node.band_n_member())
        ]
        for _ in range(depth):
            node = node.delete()
            sched = node.band_get_partial_schedule()
            mupa = mupa.flat_range_product(sched)
            for i in range(node.band_n_member()):
                coincidents.append(1 if node.band_member_get_coincident(i) else 0)
        node = node.delete().insert_partial_schedule(mupa)
        for i, flag in enumerate(coincidents):
            node = node.band_member_set_coincident(i, flag)
        schedule = node.get_schedule()
        root = schedule.get_root()
    return schedule


def schedule_collapse_all_band(schedule: I.Schedule) -> I.Schedule:
    """
    Collapse affine-coalesced bands into separated bands.
    This is a conservative no-op fallback when affine analysis is unavailable.
    """
    return schedule


def schedule_permute(schedule: I.Schedule, n: int, perms: List[int]) -> I.Schedule:
    root = schedule.get_root().first_child().child(n)
    sequence: Optional[I.ScheduleNode] = None
    bands: List[I.MultiUnionPwAff] = []

    def explore(node: I.ScheduleNode) -> None:
        nonlocal sequence
        typ = node.get_type_name()
        if typ in ("sequence", "set"):
            if sequence is None:
                sequence = node
            for i in range(node.n_children()):
                child = node.child(i)
                if child.first_child().get_type_name() == "band":
                    explore(child.first_child())
                    return
        elif typ == "band":
            bands.append(node.band_get_partial_schedule())
            if len(bands) < len(perms):
                explore(node.first_child())
        elif typ == "leaf":
            return
        else:
            explore(node.first_child())

    explore(root)
    if not bands:
        return schedule
    bands = permute_list(perms, bands)
    root = root.first_child().delete().cut()
    for band in bands:
        root = root.first_child().insert_partial_schedule(band)
    if sequence is not None:
        uset_list = I.UnionSetList.alloc(0)
        for f in schedule_node_sequence_get_filters(sequence):
            uset_list = uset_list.add(f)
        root = root.insert_sequence(uset_list)
    return root.get_schedule()


def schedule_node_band_delete_on_sequence(sequence: I.ScheduleNode, pos: int) -> I.ScheduleNode:
    band = sequence.child(pos).child(0)
    if band.get_type_name() != "band":
        return sequence
    return band.delete().parent().parent()


def schedule_node_insert_subtree(node: I.ScheduleNode, subtree: I.ScheduleNode) -> I.ScheduleNode:
    def clone_into(dst: I.ScheduleNode, src: I.ScheduleNode) -> I.ScheduleNode:
        typ = src.get_type_name()
        if typ in ("sequence", "set"):
            filters = [src.child(i).filter_get_filter() for i in range(src.n_children())]
            uset_list = I.UnionSetList.alloc(0)
            for f in filters:
                uset_list = uset_list.add(f)
            dst = dst.insert_sequence(uset_list) if typ == "sequence" else dst.insert_set(uset_list)
            for i in range(src.n_children()):
                child_src = src.child(i).child(0)
                if child_src.get_type_name() != "leaf":
                    child_dst = dst.child(i).child(0)
                    dst = clone_into(child_dst, child_src).parent().parent()
            return dst
        if typ == "filter":
            uset_list = I.UnionSetList.alloc(0).add(src.filter_get_filter())
            dst = dst.insert_sequence(uset_list).child(0)
            return clone_into(dst.child(0), src.child(0))
        if typ == "band":
            dst = dst.insert_partial_schedule(src.band_get_partial_schedule())
            return clone_into(dst.child(0), src.child(0))
        return dst

    return clone_into(node, subtree)


from caten.polyhedral.codegen import Directive, DirectiveParams


class ParallelParams(DirectiveParams):
    sizes: List[int]


class Parallel(Directive):
    name = "Parallel"
    Params = ParallelParams

    def __init__(self, *sizes: int, tile_sizes: Optional[List[int]] = None) -> None:
        if sizes and tile_sizes is not None:
            raise ValueError("Parallel accepts either positional sizes or tile_sizes keyword.")
        if tile_sizes is not None:
            normalized = list(tile_sizes)
        elif len(sizes) == 1 and isinstance(sizes[0], (list, tuple)):
            normalized = list(sizes[0])
        else:
            normalized = list(sizes)
        super().__init__(sizes=normalized)

    def on_schedule(self, node: I.ScheduleNode) -> I.ScheduleNode:
        band_paths = schedule_gather_path(node, lambda n, _p: n.get_type_name() == "band")
        if not band_paths:
            return super().on_schedule(node)
        band = schedule_node_at_path(node, band_paths[0])
        sizes = list(self.params.sizes) if self.params is not None else []
        if not sizes:
            return super().on_schedule(node)
        if len(sizes) == 1:
            sizes = sizes * band.band_get_space().dim(3)
        mv = _multival_from_sizes(band, sizes)
        band = band.band_tile(mv)
        return band.insert_mark(self.mark_id())
