# Python Coding Directives for Caten.py

- Target abstraction: provide a high-level ISL wrapper exposed as `import caten.isl as I` per README examples; prioritize architecture over ad-hoc code, mirroring the Common Lisp reference in `caten_isl_ref/*.lisp` when designing Python layers.
- Automatic ownership semantics: model `__isl_give/__isl_take/__isl_keep` by inserting copies for destructive APIs, rely on GC-driven finalizers for freeing handles (no context-driven bulk free), and keep `I.InPlace(obj)` available for explicit destructive use-cases like ScheduleTree edits.
- Context requirements: implement `with I.context()` as a mandatory, thread-safe scope injector that validates allocations/copies and (de)serializes primitive contexts, but never directly manages lifetime.
- Context helper usage: call `current()` directly instead of layering helper wrappers so implementations stay minimal and beautiful.
- Object layer expectations: define `ISLObject` subclasses (e.g., `Set`, `UnionSet`) one layer above `islpy` objects, offering operator overloading via multi-dispatch (e.g., `Set + Set`, `Set + Constraint`) and capturing allocation metadata through `copy/free/from_ptr` hooks.
- Function layer requirements: engineer `ISLFunction` decorators plus qualifiers (`I.Keep`, `I.Give`, `I.Take`, `I.Null`, `I.Param`) so Python wrappers can infer argument modes, enforce class/type checks, auto-inject contexts, and wrap primitive return values.
- Code quality priorities: “abstraction first” with well-typed, dense Python—smart algorithms and succinct comments only when truly helpful; defer detailed API wrappers until the foundational abstractions are solid.
- Module exports: avoid defining `__all__`; instead, expose the intended public symbols by importing them in the package-level `__init__.py` for each directory, so downstream code can rely solely on `import caten.isl as I` and reference everything as `I.context`, `I.Set`, `I.Take`, … without touching submodules directly.
- Testing allowance: lightweight test doubles (e.g., in `test/test_isl_gc.py`) may define minimal `isl_set`-like functions/objects to validate GC behavior without bringing real `islpy` bindings.
- Documentation: adopt **Sphinx** for project-wide docs, and ensure new public classes/methods include meaningful docstrings so Sphinx autodoc output remains useful.
