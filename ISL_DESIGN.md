# ISL Wrapper Design

This document captures the architecture plan for `import caten.isl as I`, a high-level wrapper over `islpy` that mirrors the Common Lisp prototype in `caten_isl_ref/*.lisp` while embracing Pythonic ergonomics.

## Goals
- Provide predictable memory discipline on top of `islpy`, automatically inserting copies for destructive APIs and freeing temporary objects via GC-driven finalizers (context merely scopes legality, not lifetime).
- Offer an idiomatic Python facade (`I.Set`, `I.UnionSet`, etc.) featuring multi-dispatch arithmetic (`Set + Constraint`, `Set + Set`, …) and convenient construction from strings.
- Preserve access to destructive operations (e.g., ScheduleTree editing) via explicit opt-in handles such as `I.InPlace(obj)`.
- Maintain strong typing and abstraction boundaries so higher-level modules (`caten.polyhedral`, schedulers) can rely on predictable behavior.
- Keep the implementation dense but readable: abstraction first, well-typed, minimal boilerplate, leverage meta-programming where helpful.

## Layered Architecture
1. **Primitive Layer (`IPrim`)** – Thin, possibly auto-generated bindings around `islpy` that expose raw handles and primitive functions (copy/free/read_from_str/etc.).
2. **Context Layer (`ISLContext`, `I.context()`)** – Provides thread-local scoping and access to primitive contexts; it validates operations happen under `with I.context()` but defers cleanup to object-level finalizers.
3. **Object Layer (`ISLObject` and subclasses)** – Python objects that wrap primitive handles, enforce context membership, and surface `copy()/free()` semantics.
4. **Function Layer (`ISLFunction` decorators)** – Declarative description of argument/return qualifiers (`I.Take`, `I.Give`, `I.Keep`, `I.Param`, `I.Null`, `I.InPlace`) that synthesize wrappers calling the primitive layer safely.
5. **High-Level API (`Set`, `UnionSet`, …)** – Multi-dispatch algebra, user-facing constructors, and ergonomic helpers.

## Context Management (`caten/isl/context.py`)
- `ISLContext` implements `__enter__/__exit__`, creating the underlying `islpy.Context` (if needed) and pushing itself onto a thread-local stack (`contextvars.ContextVar`).
- It intentionally does **not** own object lifetimes—GC/finalizers free handles—so exiting a context only tears down the primitive handle and pops the stack.
- Helper APIs:
  - `ISLContext.current(required=True)` – fetch current context, failing with a descriptive error when `required`.
  - (Later) `ctx.attach_handle(...)` helpers may exist purely for ergonomics, not lifecycle management.
- Contexts are strictly necessary: constructors or copies performed without an active context raise, preventing untracked allocations.

## Object Model (`caten/isl/obj.py`)
- `ISLObject` is an abstract base class with:
  - `handle`: opaque primitive pointer tracked via `weakref.finalize`.
  - Flags such as `_in_place`/`_pending_give` to emulate `__isl_take/__isl_keep` semantics without storing contexts.
  - Abstract hooks `copy(self) -> Self` and class-level `free_handle(handle)` used by GC finalizers; explicit `free()` simply triggers the finalizer early.
- Class-level metadata (e.g., `_prim_copy`, `_prim_free`, `_prim_make_from_str`) is registered in a metaclass, akin to `define-isl-object`. This metadata allows automatic generation of `from_str`, `to_str`, and list-type helpers.
- Construction helpers:
  - `ISLObject.from_ptr(cls, handle)` – wraps a raw pointer while inheriting the current context requirement from higher layers.
  - `obj.as_take()` – enforces `__isl_take` semantics: returns self if `no_copy_for_next_take`, otherwise returns `self.copy()`.
  - `obj.as_keep()` – returns self but validates that the object isn’t compromised.
- `I.InPlace(obj)` is implemented as a lightweight proxy (`ISLInPlaceView`) that flips `no_copy_for_next_take` and tracks compromised state once consumed.

## Function Binding (`caten/isl/func.py`)
- `ISLFunction` acts both as registry and decorator:
  - `@ISLFunction.redefine("isl_union_map_apply_range")` inspects the wrapped Python function signature and annotations.
  - Qualifier annotations (`I.Take(IPrim.UnionMap)`, `I.Give(...)`, `I.Param(int)`, `I.Null()`) describe how each argument should be handled.
  - During registration, a wrapper is synthesized: it validates runtime types, injects the current context, performs the necessary `.as_take()` / `.as_keep()` / parameter conversions, calls the `islpy` primitive, then post-processes the result via `infer_result_wrapper` (similar to the Lisp macro).
  - Return annotations drive wrapping: `I.Take(IPrim.UnionMap)` means the primitive returns a give-pointer and the wrapper should call `ISLObject.from_ptr` on the corresponding class.
- The decorator keeps metadata (original name, primitive symbol, argument descriptors) for documentation and potential auto-generation of higher-level operators.
- Support utilities:
  - `class ISLQualifier`: base for `Take/Give/Keep/Param/Null/Context`. Each exposes `prepare(value, idx, ctx)` and `wrap(result, ctx)` hooks.
  - Error reporting is precise (argument index, expected class, context state) to ease debugging.

## Memory Discipline
- Every `ISLObject` installs a `weakref.finalize` hook that calls the subclass-provided `free_handle`. Garbage collection is therefore the primary freeing mechanism; `ISLContext` simply enforces scoping and provides primitive handles.
- Destructive APIs:
  - By default, passing an object into a `Take` slot clones it (`copy()`) before giving the pointer to `islpy`, ensuring the caller retains an uncompromised copy.
  - Users can opt into destructive semantics via `I.InPlace(obj)` or by calling `.compromise()` explicitly; once compromised, the wrapper prohibits further use unless reinitialized.
- Non-ISL parameters (`I.Param`) are passed through after type validation, enabling functions that accept Python primitives or other wrappers.

## High-Level Pythonic API (`caten/isl/specs/*.py`)
- Each user-facing class (`Set`, `UnionSet`, `Constraint`, etc.) subclasses `ISLObject` and plugs into the metadata table, exposing constructors such as `Set.from_str("{ S[i] : 0 <= i < n }")`.
- Operator overloading relies on multi-dispatch:
  - Use `functools.singledispatchmethod` or a custom registry keyed by `(lhs_cls, op, rhs_cls)` to resolve to the correct `ISLFunction` wrapper.
  - Example: `Set.__add__(self, other)` dispatches on type of `other` and selects between `isl_union_set_union`, `isl_set_add_constraint`, etc., automatically handling qualifiers.
- Chaining with Python objects is supported via `I.Param`: e.g., `Set & "0 <= i < N"` wraps the string into a temporary `Constraint` via the registered constructor.
- ScheduleTree and other advanced structures will reuse the same layers, benefitting from consistent context enforcement.

## Implementation Roadmap
1. **Scaffolding** – Implement `ISLContext` (thread-local gating) and minimal `ISLObject` base with GC finalizers.
2. **Qualifier System** – Define `I.Take/Give/Keep/Param/Null/InPlace` dataclasses plus runtime helpers.
3. **ISLFunction Decorator** – Build the registry, argument parser, and wrapper synthesis (initially manual, later auto-generated from specs if desired).
4. **Core Object Types** – Port essential ISL objects (Set, UnionSet, Map, Constraint) using metadata similar to `define-isl-object`.
5. **Operator Layer** – Introduce multi-dispatch arithmetic and friendly constructors inside `caten/isl/specs/`.
6. **Advanced Features** – ScheduleTree operations, context-aware auto-GC tuning, optional tracing/logging.

## Open Questions
- How much of `islpy` should be auto-generated from `caten_isl_ref/{function-specs,object-specs}.lisp` versus hand-written? A script may parse these files to minimize drift.
- Should contexts be nestable with resource inheritance (child contexts reusing parent handles), or should each context be isolated? Current assumption: stack discipline with inheritance of the primitive `islpy.Context` handle.
- Error handling strategy when finalizers fail (e.g., double free) – currently logged via `warnings`, but should configurable escalation exist?
- How to expose tracing/profiling hooks for debugging context leaks without penalizing release builds?
