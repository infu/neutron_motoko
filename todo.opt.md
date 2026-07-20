# Browser Compiler Optimization TODO

## Goal

Make the wasm_of_ocaml Motoko compiler reliably compile a representative
Neutron kernel with roughly 50 installed app backends in stock Chrome, without
`--js-flags=--stack-size=...`.

Neutron app backends are not independent canisters. Each package contributes
content-addressed Motoko modules which are imported into one generated
`neutron.mo` actor. Every install or uninstall recompiles the kernel, every
target app backend, their shared dependencies, generated actor wrappers, and
the complete stable-memory shape into one canister Wasm.

There are two different Wasm modules involved:

- `moc_wasm` is the OCaml compiler built with wasm_of_ocaml and running inside
  Chrome. Its stack, heap, encoder, and JS bridge are the focus of this plan.
- The compiler emits the combined Neutron canister Wasm. Its byte size and
  instruction count drive compiler work, but its runtime stack is unrelated to
  the Chrome `RangeError` seen during compilation.

The current reference workload is approximately 12 manifests, 170 stored
modules, 167 reachable modules, 1.35 million source characters, about 409
explicit security-analysis parses, and a 2.47 MB emitted canister Wasm. App
count is only a convenient scale label; all acceptance results must also record
unique modules, source bytes, generated actor size, diagnostics, and output
Wasm bytes.

## Guardrails

- Preserve generated actor behavior, Candid, stable signatures, migration
  checks, dependency ordering, security findings, and warning severity.
- Do not globally hide warnings. Neutron relies on actionable diagnostics such
  as `M0207` to detect unplanned stable-data loss.
- Preserve deterministic output. Prefer byte-identical Wasm for refactors; any
  intentional byte difference needs a decoded structural comparison and
  runtime-equivalence coverage.
- Do not introduce persistent typed-AST, IR, or cross-compilation caches in
  this plan. Content-sensitive compiler caching is a separate design problem.
- Do not make a Service Worker or a Chrome launch flag part of correctness.
  A per-operation Dedicated Worker may be used later for UI isolation and heap
  reclamation, after the compiler paths below are stack-safe.
- Keep each numbered item independently reviewable and commit it separately.

## 1. Add a Wasm-only encoder path

The browser API currently calls `CustomModuleEncode.encode`, discards its
source-map result, and keeps only the Wasm. The encoder still records mappings
for source-located instructions, recursively searches the growing source list,
reads and retains complete source contents, builds debug/source sets, and
serializes source-map JSON.

Implementation:

- [ ] Snapshot two independent modes at encoder entry: `emit_source_map`,
  supplied by the caller, and `emit_debug_info`, derived from the requested
  compiler mode. Never collapse them into one metadata switch: `mo-ld` needs
  DWARF while discarding the source map, and `moc --map` may request a map
  without DWARF.
- [ ] Refactor stream finalization into `to_bytes`: copy the `Buffer` once,
  apply patches to that owned `bytes`, and do not mutate it afterward.
- [ ] Add final no-map APIs in `src/wasm-exts/customModuleEncode.mli` and
  `src/wasm-exts/customModuleEncode.ml`: a byte-returning entry point for the
  browser and a string-returning wrapper for native/internal consumers.
  Preserve the existing map-producing API as a compatibility wrapper; do not
  introduce an intermediate no-map string API that item 5 immediately changes.
- [ ] When source maps are disabled, do not allocate map state. Bypass
  `add_source`, `add_to_map`, source-content reads, VLQ writes, mapping-buffer
  writes, and final Yojson construction entirely.
- [ ] When debug information is disabled, bypass the note callback,
  `statement_positions`, `instr_notes`, `source_names`,
  `source_path_indices`, `sequence_bounds`, DWARF strings/tags/promises, and
  all `Meta Tag`, `TagClose`, `Grouped`, and `StatementDelimiter` processing.
  `Meta` instructions remain non-emitting.
- [ ] Keep source-map and DWARF collection independently selectable. Preserve
  every ordinary instruction byte and all non-debug custom sections, including
  an explicitly supplied `sourceMappingURL`.
- [ ] Route `src/js/common.ml` to the byte-returning no-map entry point.
- [ ] Route `compile_unit_to_wasm` in `src/pipeline/pipeline.ml` and `mo-ld`
  through the no-map string wrapper; `mo-ld` must continue emitting DWARF.
- [ ] Make `src/exes/moc.ml` use the map-producing path only for `--map` and the
  no-map string wrapper otherwise.
- [ ] In the retained map path, replace recursive filename lookup with a hash
  table while retaining first-seen order in separate linear accumulators. Keep
  `sources` and `sourcesContent` index-aligned and close source channels with
  exception-safe cleanup.

Acceptance:

- [ ] For the same `extended_module`, map-enabled and map-disabled modes emit
  identical Wasm bytes with debug off and with debug on.
- [ ] Cover the four-mode matrix: map off/debug off, map on/debug off, map
  off/debug on, and map on/debug on.
- [ ] `moc --map` emits a valid version-3 map with deterministic first-seen
  source ordering and aligned `sources`/`sourcesContent`; plain `moc` performs
  no source read or map JSON serialization.
- [ ] `mo-ld` emits the same debug sections while performing no source-map
  work.
- [ ] An instrumented encoder test proves that the no-map path calls neither
  the source loader nor the map serializer. An empty returned map is
  insufficient because the current source-read failures are swallowed.
- [ ] Classical and enhanced orthogonal-persistence fixtures exercise the
  map/debug modes and compare Wasm bytes, source maps, `sourceMappingURL`, and
  debug sections separately.

## 2. Make `InstrList` materialization stack-safe

`src/codegen/instrList.ml` represents instruction sequences as composed
functions. `(^^)`, `concat`, `concat_map`, and `concat_mapi` build nested
closures; materializing a large generated initializer or serializer can then
consume stack proportional to the number of fragments.

Implementation:

- [ ] Replace the functional difference-list representation with an explicit
  append tree/rope or another builder whose concatenation is O(1) without
  adding a native/Wasm call frame per fragment.
- [ ] Make `concat`, `concat_map`, `concat_mapi`, and `table` construct that
  representation with tail-recursive loops. Do not retain the current
  `List.map` plus `List.fold_right` intermediate-list paths.
- [ ] Flatten append nodes with an explicit work stack. Do not recursively walk
  a left- or right-skewed append tree.
- [ ] Represent region changes, nested blocks, `if`, loops, remembered label
  depths, and instruction leaves explicitly enough to preserve their current
  evaluation and emission order.
- [ ] Model `remember_depth` with an explicit enter frame that fulfills its
  `Lib.Promise` before the work stack enters the remembered body. A
  corresponding `branch_to_` must never be evaluated first.
- [ ] Provide explicit raw and optimized materialization helpers. Preserve the
  current distinction: top-level and nested Wasm bodies are optimized, while
  `setter_for` and `is_nop` inspect the raw sequence.
- [ ] Route `setter_for`, `is_nop`, `to_instr_list`, and nested-body
  materialization through those stack-safe helpers.
- [ ] Keep the existing peephole optimizer semantics and source regions.

Acceptance:

- [ ] Materializing at least 100,000 concatenated instruction leaves succeeds
  without a stack overflow.
- [ ] Deeply skewed `concat`, `concat_map`, and direct `(^^)` construction all
  remain bounded.
- [ ] Tests cover nested blocks, loops, `if` branches, remembered depths,
  forward/backward branches, source regions, and `setter_for`.
- [ ] Representative classical and enhanced compiler fixtures produce
  byte-identical Wasm, or any reviewed difference is structurally and
  behaviorally equivalent.

## 3. Analyze each unique Neutron module once

Neutron currently parses the union graph for reachability, then reparses shared
dependencies within each app's danger-scan closure. The parser exports a full
JS AST containing source/doc wrappers and non-enumerable `rawExp` references to
the original OCaml expressions, after which Neutron builds another simplified
JS tree. The security policy only needs imports and a small set of syntax facts.

This item spans this compiler repository and the Neutron integration. Keep the
compiler API generic; Neutron remains responsible for deciding which syntax
facts are forbidden.

Implementation:

- [ ] First refactor Neutron's reachability and danger analysis into one union
  module graph keyed by the content-addressed module path. Store compact
  `{ imports, findings }` data and discard each AST immediately.
- [ ] Preserve per-app attribution by walking the compact graph from each app's
  roots. The global reachability walk still follows imports through a
  whitelisted module because the compiler needs them; each app's danger walk
  records that whitelisted module with no findings and stops at that boundary,
  exactly as it does now. Preserve migration/schema roots, missing-module
  errors, and deterministic report ordering.
- [ ] Replace the repeated recursive security-rule traversals plus separate
  import traversal with one iterative AST visit that collects imports and all
  security facts.
- [ ] Add a compiler `inspectMotoko`-style API that parses a named source and
  returns only immediate raw import strings in source order and compact syntax
  facts needed by callers. For the current rules, the compact facts are an
  actor-URL presence bit and the unique exact member names encountered in
  `DotE`; Neutron maps those facts to its existing ordered rule names.
- [ ] Keep this inspection parse independent of dependency resolution so
  reachability can run before files are installed in the compiler VFS. Do not
  silently replace raw `ImportE` strings with `resolved_import_name` results;
  preserve Neutron's current filtering of `mo:⛔` and `mo:prim`.
- [ ] The compact response must not export editor types, docs, parent links,
  source wrappers, full AST nodes, or `rawExp` objects.
- [ ] Expose that method through `src/js/moc_wasm.ml` and the
  `neutron-motoko-wasm` wrapper, then switch the Neutron graph analysis to it.
- [ ] Keep the normal full AST APIs intact for language-server/editor callers.
- [ ] Do not retain inspection results across compile operations in mot_comp;
  content-addressed deduplication for this item belongs to the current Neutron
  operation.

Acceptance:

- [ ] The number of explicit inspection parses equals the number of unique
  reachable modules, not the sum of every app dependency closure.
- [ ] Shared dependencies imported by many apps are inspected once but receive
  the same per-app finding attribution as before.
- [ ] Tests cover all existing forbidden patterns, whitelisted modules,
  multiple app roots, schema and migration roots, cycles, and missing imports.
- [ ] Parse failures and missing-module failures retain the same public error
  category and ordering; the refactor must not turn them into partial reports.
- [ ] Old and new danger reports are deeply equal on the current installed-app
  fixture.
- [ ] The compact result contains no `rawExp` reference or full mirrored AST.

## 4. Fix browser-VFS warnings and make diagnostics stack-safe

The browser virtual filesystem currently produces an `M0005` warning for most
imported files. The check opens a file successfully and then fails to find the
same basename through `Sys.readdir`. In addition, `Diag.warn` does not honor
disabled warning levels on its direct result path, and diagnostics are exported
with recursive list mapping.

Implementation:

- [ ] Correct the VFS/case-check boundary so an exact virtual-file open does
  not emit `M0005`. Preserve a real case-mismatch warning on native
  case-insensitive filesystems.
- [ ] Prefer warning only when the directory contains a case-insensitive match
  with different spelling; do not turn an unrelated or incomplete directory
  listing into a generic case warning.
- [ ] If `Sys.readdir` is unsupported or incomplete for a filesystem adapter,
  treat a successful exact open as authoritative. Remove the current generic
  "located with a different name" fallback instead of replacing it with a new
  browser-specific warning.
- [ ] Make direct `Diag.warn` results honor `Flags.is_warning_disabled`, matching
  the documented `-A <code>` behavior and the existing `add_msg` path.
- [ ] Replace `Array.of_list (List.map diagnostics_of_msg msgs)` in
  `src/js/common.ml` with an explicitly allocated array filled iteratively.
- [ ] Rewrite `Diag.traverse`, `Diag.traverse_`, and `Diag.fold` with
  tail-safe/reverse accumulators while preserving left-to-right execution,
  message ordering, and first-error behavior.
- [ ] Avoid redesigning the complete `Diag.result` representation in this item;
  address only proven long-list paths.
- [ ] Do not use `--hide-warnings` in Neutron.

Acceptance:

- [ ] A browser-VFS compile produces no false per-module `M0005` warnings.
- [ ] A genuine supported case mismatch still reports `M0005`.
- [ ] `-A M0005` suppresses that warning consistently without suppressing
  errors or unrelated warnings.
- [ ] `M0207` still reaches Neutron with the same code, severity, source, and
  message and still blocks unplanned data loss.
- [ ] Large synthetic diagnostic and traversal lists complete without stack
  overflow, preserve exact value/message ordering, and stop at the same first
  error without evaluating later callbacks.

## 5. Replace byte-at-a-time Wasm export with a bulk transfer

`src/js/common.ml` currently creates the returned `Uint8Array` with a callback
that reads one OCaml string byte per element. Output cost therefore includes
one JS-to-compiler-Wasm callback for every emitted canister byte.

Implementation:

- [ ] Treat the owned, byte-returning no-map encoder API from item 1 as this
  item's input; do not change the encoder output representation again here.
- [ ] Convert the encoder's `bytes` with
  `Js_of_ocaml.Typed_array.Bytes.to_uint8Array`, which is available to the
  pinned wasm_of_ocaml-compatible JS library, rather than inventing a custom
  per-byte primitive.
- [ ] Treat that typed-array result as borrowed and possibly aliased. Always
  construct a new `Uint8Array` from it before exposing the result. The returned
  view must own an exact-sized `ArrayBuffer` with `byteOffset === 0`.
- [ ] Do not use a Bigarray bridge unless the encoder itself is changed to
  write directly into a Bigarray; copying the completed `Buffer` into a
  Bigarray only adds another full-size copy.
- [ ] Remove the `Uint8Array.from(arrayLike, perByteCallback)` path.
- [ ] Make the returned buffer suitable for transfer from a future Dedicated
  Worker without an additional element-by-element conversion.

Acceptance:

- [ ] The wasm_of_ocaml artifact, not only the `moc.js` target, returns a real
  `Uint8Array`; its first eight bytes are the Wasm magic and version and its
  complete contents match the legacy conversion hash.
- [ ] Cover empty and small byte arrays, values around `0x7f`, `0x80`, and
  `0xff`, and a representative Neutron output.
- [ ] `WebAssembly.validate` or `WebAssembly.compile` accepts the returned
  bytes.
- [ ] Mutating every region of a returned buffer cannot affect compiler state
  or the bytes produced by a later compile.
- [ ] A first result remains unchanged after another compile and an explicit
  compiler-GC opportunity.
- [ ] Transferring the returned `ArrayBuffer` detaches only the caller's result;
  a subsequent compile still succeeds with correct bytes.
- [ ] The returned view has `byteOffset === 0` and
  `byteLength === buffer.byteLength`.
- [ ] Generated glue contains no per-element callback path. Benchmark JS
  export separately from encoding and confirm one bridge conversion plus one
  bulk ownership copy.

## 6. Remove quadratic compiler list construction

`compile_libs` in `src/pipeline/pipeline.ml` repeatedly evaluates
`imports @ new_imports`, copying the complete accumulated prefix for every
library. Other numbered items handle source-map and diagnostic lists; this item
focuses on remaining compiler pipeline accumulation.

Implementation:

- [ ] Maintain `rev_imports` and add each forward-ordered `new_imports` with
  `List.rev_append new_imports rev_imports`; reverse once at the final
  consumer.
- [ ] Before compiling an imported actor class, materialize the required
  forward-ordered imports exactly once for that boundary, then resume reverse
  accumulation.
- [ ] Preserve provider-before-consumer ordering and the existing treatment of
  modules, mixins, and actor classes.
- [ ] Audit adjacent module-count-scaled `@`, `List.fold_right`, and repeated
  `List.nth` paths reached by `compile_files`; convert only paths whose input
  grows with the complete imported graph.
- [ ] Keep changes separate from the linker remapping work in item 7.

Acceptance:

- [ ] Import declaration construction is linear for a graph of ordinary module
  libraries.
- [ ] Mixed module/mixin/actor-class fixtures preserve import and initialization
  order.
- [ ] Missing imports, cycles, and provider ordering produce the same results
  and diagnostics.
- [ ] Large library lists complete without stack overflow.

## 7. Consolidate full-module linker traversals

`src/linking/linkModule.ml` separately walks and copies large function bodies
to rename type, function, and global indices, followed by additional section
rewrites. This multiplies peak allocations and work as the emitted combined
actor grows.

Implementation:

- [ ] Introduce one pure module remapper with identity-default mappings for the
  index spaces the current linker actually renames: types, functions, and
  globals. Add another index space only when replacing an existing pass that
  already rewrites it; do not broaden linker behavior for completeness.
- [ ] Start with identity mappings and golden tests, then replace the separate
  type/function/global instruction traversals one at a time.
- [ ] Traverse each function body once for the renamings being applied at that
  linker stage. Preserve nested block structure and every source region.
- [ ] Use an explicit instruction work stack so both long flat bodies and
  deeply nested block/loop/`if` trees are stack-bounded; a single recursive
  visitor is still insufficient in the browser compiler.
- [ ] Fuse only mappings that are simultaneously valid at the same linker
  stage. Keep base-import replacement, import removal, GOT.mem one-to-many
  instruction expansion, and post-merge GOT replacement at their current
  semantic boundaries until separate equivalence tests justify a change.
- [ ] Build rewritten sections with linear accumulators rather than repeated
  append operations.
- [ ] Preserve GOT function/data handling, imported-index offsets, Memory32 and
  Memory64 behavior, passive/active data-segment numbering, start-function
  injection, name-section indices, and export filtering.
- [ ] Do not introduce separately compiled per-app Wasm objects or a new linker
  architecture in this item.

Acceptance:

- [ ] Existing linker fixtures remain byte-identical where ordering is not
  intentionally changed.
- [ ] Structural decode comparisons cover every reviewed byte difference.
- [ ] Tests cover type/function/global renaming, GOT.func, GOT.mem, actor-class
  linking, Memory32/Memory64, passive data segments, start injection, names,
  and debug/no-debug modules.
- [ ] Each compatible remapping stage performs one instruction-tree traversal
  rather than separate complete passes for each index space.
- [ ] Synthetic long-flat and deeply nested function bodies remap without a
  stack overflow and retain exact instruction/source-region order.

## Cross-cutting verification gate

- [ ] Capture pre-change Candid, stable signature, diagnostics, danger report,
  deployment id, reachable module list, and Wasm hashes for representative
  install and uninstall inputs.
- [ ] Extend the actual wasm_of_ocaml smoke/browser test; do not rely on
  `test/test-moc.js`, which exercises the JavaScript compiler target.
- [ ] Record the locked nixpkgs revision, resolved js_of_ocaml/wasm_of_ocaml
  versions, compiler asset hash, Chrome version, and whether each run uses a
  cold or warm compiler instance.
- [ ] Keep focused unit/golden tests with each numbered commit; do not defer all
  validation to the final scale test.
- [ ] Add phase timing and peak-memory observations for parse/inspect, typecheck,
  codegen, link, encode, and JS export so later work targets measured costs.
- [ ] Run a cold compile of a representative roughly-50-app combined kernel in
  ordinary Chrome with no custom V8 stack flag.
- [ ] Repeat the same compile in a fresh browser context to prove success does
  not depend on JIT warm-up or retained compiler state.
- [ ] Verify install and uninstall stable compatibility and emitted actor
  behavior, not merely successful Wasm generation.
- [ ] Compare plain `moc` and `moc --map` structurally: the CLI intentionally
  sets `sourceMappingURL` only for `--map`, so those command outputs need not be
  byte-identical even though encoder modes must match for the same input
  `extended_module`.
- [ ] Treat app count as reporting metadata; enforce and publish the associated
  unique-module, source-byte, generated-source, diagnostic, and output-Wasm
  measurements.
