# Python Coding Directives for Caten.py

- Target abstraction: provide a high-level ISL wrapper exposed as `import caten.isl as I` per README examples; prioritize architecture over ad-hoc code, mirroring the Common Lisp reference in `caten_isl_ref/*.lisp` when designing Python layers.
- Automatic ownership semantics: model `__isl_give/__isl_take/__isl_keep` by inserting copies for destructive APIs, rely on GC-driven finalizers for freeing handles (no context-driven bulk free), and keep `I.InPlace(obj)` available for explicit destructive use-cases like ScheduleTree edits.
- Context requirements: implement `with I.context()` as a mandatory, thread-safe scope injector that validates allocations/copies and (de)serializes primitive contexts, but never directly manages lifetime.
- Context helper usage: call `current()` directly instead of layering helper wrappers so implementations stay minimal and beautiful.
- ctypes configuration: avoid setting `restype`/`argtypes` manually; route bindings through `ISLFunction.create` so signatures stay centralized.
- Type discipline: skip `isinstance` runtime checks for ISL wrappers unless absolutely necessary—prefer static typing/multi-dispatch plans, and defer operator overloads (e.g., `__or__`) until the shared mixin layer lands.
- Testing: use `pytest` with the global config enforcing coverage ≥ 95%, keeping new suites aligned with the existing `pytest.ini_options` defaults.
- Object layer expectations: define `ISLObject` subclasses (e.g., `Set`, `UnionSet`) one layer above `islpy` objects, offering operator overloading via multi-dispatch (e.g., `Set + Set`, `Set + Constraint`) and capturing allocation metadata through `copy/free/from_ptr` hooks.
- Function layer requirements: engineer `ISLFunction` decorators plus qualifiers (`I.Keep`, `I.Give`, `I.Take`, `I.Null`, `I.Param`) so Python wrappers can infer argument modes, enforce class/type checks, auto-inject contexts, and wrap primitive return values.
- Code quality priorities: “abstraction first” with well-typed, dense Python—smart algorithms and succinct comments only when truly helpful; defer detailed API wrappers until the foundational abstractions are solid.
- Module exports: avoid defining `__all__`; instead, expose the intended public symbols by importing them in the package-level `__init__.py` for each directory, so downstream code can rely solely on `import caten.isl as I` and reference everything as `I.context`, `I.Set`, `I.Take`, … without touching submodules directly.
- Testing allowance: lightweight test doubles (e.g., in `test/test_isl_gc.py`) may define minimal `isl_set`-like functions/objects to validate GC behavior without bringing real `islpy` bindings.
- Documentation: adopt **Sphinx** for project-wide docs, and ensure new public classes/methods include meaningful docstrings so Sphinx autodoc output remains useful.

## Wrapper Construction Instruction
- Think in English, answer in Japanese.
- 作業はデータ型ごとに分割（Set, UnionSet …）。コード自動生成は禁止。`./caten/isl/gen/*.json` の Spec を手作業で確認し、`./docs/ISL_APIS.md` のチェックボックスを実装完了ごとに更新。
- Wrapper は必ず `ISLFunction.create` を経由し、`lib` に関数定義を直接挿入しない。メソッド名は `isl_` プレフィックスやオブジェクト名を削除してクラスメソッドから到達可能にする（例: `isl_union_map_add_union` → `add`）。
- Set のみで完結する API は `caten/isl/specs/set.py` に集約。UnionSet 依存 API は名称だけ残し未実装可。Constraint 依存 API も未実装で良いが場所はここに置かない。既存コードへ不要な変更を加えない。
- `Set` の文字列表現は `_isl_set_to_str` を用いた `__str__`/`__repr__` で整形（他クラスも同習慣を踏襲）。
- `isl_set_wrapped_reverse` のような未リンクシンボル向けに `_prim` のような動的ラッパーを追加しない。存在しない場合は実装をコメントアウトするか `NotImplementedError` を返す。
- Qualifier `Param` に `bool` を追加し、`isl_bool` の -1 は `ctx.raise_isl_error()` とする。`_as_bool` のような外部ヘルパは不要。
- 返り値が NULL になり得る `Param` ではエラーチェックをしないが、`Null` 以外で None が返った場合はコンテキストがあればエラーを挙げる。
- コメントや docstring は日本語禁止（コード中）。説明は英語で。
- ruff / mypy / pytest を定期実行して整合を確認すること。代表コマンド例:
  - `UV_CACHE_DIR=./.uv-cache uv run ruff check`
  - `UV_CACHE_DIR=./.uv-cache uv run mypy caten/isl`
  - `UV_CACHE_DIR=./.uv-cache uv run python -m pytest test/isl/test_isl_context.py test/isl/test_isl_set.py --no-cov`
- `./caten/isl/gen/*.json` に全ての Spec がある。Set, UnionSet など型ごとにタスクを区切り、手作業で全 API を実装→テスト→合格確認を繰り返す。提案の拒否権なし。
- `./docs/ISL_APIS.md` にある全 ISL Function をクラス単位で整理・実装。流れは 1) 対応クラス作成 2) `ISLFunction.create` でラッパ定義（`_lib` に直接注入禁止） 3) クラスメソッド化し `isl` 接頭辞と型名は削除した名称で到達可能にする。実装済みはチェックボックスを逐次更新。
- 全関数に対しテストを用意し、ruff / mypy / pytest を定期実行。例: `uv run python -m pytest ./tests` 等。
- `./docs/ISL_APIS.md` に列挙された全 API 完了まで中断不可。
- 既存コードへの余計な修正禁止。Set 単体で完結する API は `caten/isl/specs/set.py` 内で完結させる。UnionSet 依存 API は関数名だけ置いて保留可。自動生成手法は禁止。
- 進捗と作業計画を常に本ファイルに記録し更新すること（型ごとに完了状況や今後の順番を明記）。最新の計画がここに存在する状態を保つ。

## 作業計画と進捗 (2025-11-16)
直近のギャップ集計: `docs/ISL_missing_apis.md`（2025-11-16 再生成、欠落API 2047件）。map 残 2 件（tuple_name系シンボル未提供のみ、libisl非存在）。
優先順とステータス（✅完了 / 🚧着手中 / ⏳未着手）
- Identifier / Id: 🚧（基本APIは揃うが欠落検証 継続）
- Space / LocalSpace: 🚧（dim/tuple系以外の抜け有り）
- Constraint / Equality-Constraint / Inequality-Constraint: 🚧
- BasicSet / Set: 🚧（missing計: set 105, basic_set 63）
- UnionSet: 🚧（missing計: union_set 52）
- BasicMap / Map: 🚧（missing計: basic_map 85, map 190）
- UnionMap: 🚧（missing計: union_map 112）
- Aff / PwAff / MultiAff / PwMultiAff: 🚧（missing計: aff 73, pw_aff 96, multi_aff 90, pw_multi_aff 89）
- MultiVal: 🚧（missing計: multi_val 37, val 66）
- MultiUnionPwAff / UnionPwAff / UnionPwMultiAff / MultiUnionPwAff: 🚧（missing計: multi_union_pw_aff 75 ほか）
- ScheduleConstraint / Schedule / ScheduleNode: ✅（schedule_node 0）
- UnionAccessInfo / UnionFlow: ⏳
- ASTExpr / ASTNode / ASTBuild: 🚧（Expr系クラス不足・missing計: ast_expr 0, ast_node 0）
- Mat: ✅（要素参照系API実装済・missing計: mat 0）
- その他: misc 71, options 29 など多数。

次に着手する対象:
1) ScheduleNode / ASTExpr / Mat のクラス追加・アクセサ補完
2) UnionAccessInfo / UnionFlow ラッパ実装
3) map / set 系を皮切りに `docs/ISL_missing_apis.md` に基づく欠落API埋め
