# Incremental compilation: implementation plan

Goal: the compilation server only retypes, post-processes and regenerates what
actually changed, with a result **bit-for-bit identical** to a cold compilation.

## 0. Decisions

| Topic | Decision |
|---|---|
| Inference unit | **The field.** A field's type must be fully determined at the end of its own typing. A monomorph still open in a field's type (`static var x = []`) is an **error**: a type annotation is required. |
| Granularity | **The field**, directly: dependencies, fingerprints and retyping are per field. |
| Scope | Typing, filters and generation. |
| Priority targets | **JS** and **HL**. Other targets keep a global generation step but benefit from incremental typing and filters. |
| Macros | Automatic dependency tracing, *volatile* macros re-executed with early cutoff on their output, an **aggregation API** (map/reduce) for registries, a warning on non-aggregated static writes. |
| Compatibility | Breaking changes accepted for Haxe 5, with a warning phase before errors. |
| Storage | **In memory**, preserving the physical identity of objects. hxb is not on the critical path; it remains a persistence format and a verification tool. |
| Positions in inlined code | The inliner replaces positions with the call site's (`map_pos`, `src/optimization/inline.ml`), so they are excluded from fingerprints. Exceptions: `-D keep_inline_positions` (already part of the define signature; relative positions are then included in the body fingerprint) and self-calling closures (relative positions included). |
| Validation corpus | Repository tests (`tests/unit`, `tests/server`, `tests/display`); Heaps, hxbit and domkit, for their heavy use of macros. |

## 1. Current state (starting point)

- The cache is **per module** (`module_def_extra`, `src/core/tType.ml`). Each
  dependency (`m_deps : (int, module_dep) PMap.t`) only records *which module*
  is depended upon, never *what* is used (interface, inline body, macro…).
- `check_module` (`src/compiler/server/serverCache.ml`) invalidates
  **transitively and blindly** (`DependencyDirty`): changing the body of a
  private function in a base module invalidates the whole graph above it.
- Change detection relies on modification time, or on the full parsed AST
  (`CheckFileContentModification`), which includes function bodies.
- Open monomorphs are closed to `Any` when saving
  (`update_cache_dependencies ~close_monomorphs:true`, `src/filters/filters.ml`).
  Typing results therefore already depend on the cache state.
- Post-processing is global: `Filters.run` works on `com.types`. DCE,
  `RenameVars`, `add_descendant`, `has_feature` and the analyzer's purity
  inference all operate on the whole program. Filters mutate the typed AST
  **in place**, then `save_class_state` and `cl_restore` restore it.
- Generators regenerate the whole output. `genhl` maintains global index
  tables (`cglobals`, `cfunctions`, `cstrings`, `cached_types`…).

## 2. Principles

1. **Query graph with early cutoff.** Each node produces a result and a
   **fingerprint**. Each dependency records `(node, fingerprint seen)`.
   Validating a node means re-checking its dependencies; it is only
   re-executed if one of them changed. If a re-executed node produces the same
   fingerprint, its dependents are left untouched.
2. **Separate interface from implementation**, per field and per type.
3. **Stable physical identity** of every entity another module can reference.
   Retyping a field mutates the existing object; it is never replaced by
   another object.
4. **The typed AST is immutable once produced.** Post-processing produces a
   copy, only for what changed.
5. **Determinism**: no result may depend on typing order. A verification mode
   systematically compares incremental and cold results.

## 3. Data model

### 3.1 Entities

An *entity* is an object another module can reference by pointer:

- `module_def` (its `m_id` is stable across compilations);
- module types: `tclass`, `tenum`, `tdef`, `tabstract`, as well as
  `m_statics` and `a_impl`;
- fields: `tclass_field` (including overloads and the constructor),
  `tenum_field`;
- type parameters: `typed_type_param` and their `KTypeParameter` class;
- `tanon` objects declared by typedefs.

Each entity gets a **stable key** independent of its memory address:
`(module path, type path, kind, name, overload index | parameter index)`.
The old ↔ new mapping between two typings goes through this key.

### 3.2 Nodes and fingerprints

| Node | Content | Fingerprint |
|---|---|---|
| `File(f)` | file content | content hash (modification time is only a pre-filter) |
| `Decls(m)` | parsed AST, split per type and per field | hash of the AST **without absolute positions**, per field |
| `TypeShape(t)` | path, kind, parameters, constraints, `super`/`implements`, **member set** (names, kinds, access), metadata, `@:using`, abstract underlying type, `from`/`to` | canonical hash |
| `FieldSig(cf)` | fully resolved type, kind (`var`/`method`, access), `cf_params`, metadata, relevant flags | canonical hash |
| `FieldInlineBody(cf)` | body of `inline`, `macro` and `@:generic` fields, and of abstract fields used as inline casts/operators | canonical hash of the `texpr` |
| `FieldBody(cf)` | typed `cf_expr` | canonical hash of the `texpr` (used by post-processing and generation) |
| `MacroExpansion(site)` | result of an expansion and its effects | hash of the output |
| `PostProcessed(cf)` | expression after filters | hash |
| `Fragment(t)` | generated code for a type or a function | hash |

**Canonical hasher** (new module `src/core/tFingerprint.ml`):
- module types are identified by path, never by address;
- type parameters by `(host, index)`;
- anonymous fields are sorted by name;
- bound monomorphs are followed; an unbound monomorph is forbidden in a
  signature, which phase 1 guarantees;
- `TLazy` values are forced before hashing (they already are by `PForce`);
- positions are excluded, except relative positions in the cases listed in §0;
- it writes directly into an incremental digest, without intermediate
  serialization.

### 3.3 Dependencies

The *consumer* is the node currently being computed, usually the field being
typed (`ctx.f.curfield`), or a macro expansion. Dependency kinds:

| Kind | Recorded when | Invalidated if |
|---|---|---|
| `DShape t` | iterating over or looking up members of `t`, structural unification, `using`, matcher exhaustiveness, override checks, overload resolution | `TypeShape(t)` changes |
| `DSig cf` | reading `cf.cf_type` (field access, call) | `FieldSig(cf)` changes |
| `DInline cf` | inlining, `@:generic` instantiation, macro execution (macro context) | `FieldInlineBody(cf)` or `FieldBody(cf)` changes |
| `DLookup (path, result)` | resolving a type name, an import, an `import pack.*`, sub-types | the resolution result changes, including a file that **appears** and shadows another type (negative dependency) |
| `DMacro site` | using a macro expansion | `MacroExpansion(site)` changes |
| `DFile f` | file read by a macro, `registerModuleDependency` | `File(f)` changes |
| `DDefine` | already covered by the define signature (`m_sign`) | |

Storage: `m_deps` stays as a module-level summary, for existing tools and
diagnostics. The new graph lives in a separate structure, `IncrementalGraph`,
attached to the cache context (`CompilationCache`):

```ocaml
type node_key =
  | NFile of Path.UniqueKey.t
  | NDecls of path                       (* module *)
  | NShape of entity_key
  | NSig of entity_key
  | NInline of entity_key
  | NBody of entity_key
  | NMacro of macro_site_key
  | NPost of entity_key
  | NFragment of target * entity_key

type node = {
  key : node_key;
  mutable fingerprint : Digest.t;
  mutable deps : (node_key * Digest.t) list;   (* what was read, and the fingerprint seen *)
  mutable rdeps : node_key list;               (* reverse index, for diagnostics and generation *)
  mutable verified_at : int;                   (* compilation step *)
  mutable state : Green | Red | Unknown;
}
```

## 4. Phases

Each phase can be delivered and measured on its own. Phases 0 and 1 are
independent and can start in parallel.

---

### Phase 0: verification and measurement infrastructure

Goal: be able to prove equivalence before optimizing anything.

1. **Verification mode** `-D incremental-verify`: after a server compilation,
   run a cold compilation in a fresh context and compare:
   - the fingerprints of all `Sig`, `Shape` and `Body` nodes;
   - the generated outputs, byte for byte;
   - the sorted list of errors and warnings.
2. **Stale pointer checker** `-D incremental-check-refs`: walk all cached
   modules and check that every reference to an entity of module `X` (in
   types, `texpr`, `cl_super`, `a_from_field`…) points to an object reachable
   from `X.m_types`.
3. **History replay tool** (`tests/incremental/`): a Haxe harness that starts a
   server, takes a repository and a list of commits, compiles each commit
   incrementally in verification mode, and produces a report (timings,
   recomputed nodes, differences).
   - Corpus: `tests/unit`, `tests/server`, `tests/display`; Heaps, hxbit and
     domkit (repositories cloned in CI, with replayed commits).
4. **Statistics** (`--times` and `server/stats`): nodes validated, recomputed
   and cut off by early cutoff, per node kind.

Exit criterion: the harness runs in CI against the current server
(module granularity) and reports no difference. This gives a timing baseline.

---

### Phase 1: semantic rules (Haxe 5 breaking change)

Goal: make inference local to the field.

1. **Detection.** At the end of a field's typing (after resolving the `TLazy`
   set by `typeloadFields.ml`, when `cf_type` is final), walk `cf_type` looking
   for an unbound `TMono` or an open `TAnon` (mutable `a_status`).
   - Phase 1a: warning `WUnresolvedFieldType`: "The type of `x` is not fully
     inferred, add a type annotation".
   - Phase 1b: error by default. `-D legacy-field-inference` restores the
     warning for one release.
2. **Binding instrumentation.** In `Monomorph.do_bind`
   (`src/core/tUnification.ml`), if the monomorph belongs to the signature of
   a field **other** than the one being typed, emit a diagnostic: this is the
   case the rule removes. It is used to measure the impact on the corpus before
   switching to an error.
3. **Remove** `close_monomorphs` from `update_cache_dependencies`, which becomes
   unnecessary, and the "Unclosed monomorph" error in hxb, which becomes a
   `die`.
4. **Update** the standard library, tests and corpus libraries (add
   annotations).

Cases to handle explicitly:
- `var x = null;` and `var a = [];`: error.
- `function f() return g();` where `g` is not typed yet: allowed; the type of
  `f` depends on `FieldSig(g)` (lazy typing, as today).
- Cycles of unannotated fields (`f` calls `g`, `g` calls `f`): keep the current
  behavior ("recursive type"), but record the cycle as a component for
  invalidation.
- Locals and closures: not affected, they are internal to the body.

Exit criterion: the whole CI is green with the error enabled, and the impact on
the corpus is documented.

---

### Phase 2: stable identity and content separation

Goal: be able to mutate an entity in place without breaking other modules'
pointers.

1. **Entity keys** (§3.1) and a per-module `entity_key -> object` index in
   `CompilationCache`.
2. **Make mutable** `cf_pos`, `cf_name_pos`, `ef_pos`, `mt_pos`/`cl_pos`,
   `e_pos`, `t_pos`, `a_pos` and the `*_name_pos` fields (`src/core/tType.ml`).
   `ef_index` stays immutable: an index change is a shape change.
3. **External contributions: move them out of entities, or make them
   recomputable.**
   - `cl_descendants`: recomputed by the aggregation pass (phase 7) from the
     `cl_super` and `cl_implements` of all types; no longer stored
     persistently.
   - DCE and post-processing flags (`CfUsed`, `CfPostProcessed`, …): moved to
     post-processing data (phase 7), out of `cf_flags`.
   - `m_features`: computed per module from its post-processed AST.
   - Metadata added by a macro from **another** module (`addMetadata`):
     recorded as an effect of the expansion (phase 6) and replayed.
   - `@:generic` instances: attached to the `(generic, type arguments)` node
     rather than to the generic's module (removes the `m_processed = 0`
     mechanism from #10635).
4. **Grafting** (`src/compiler/incremental/graft.ml`):
   - input: the old and new entity sets of a module;
   - entities are matched by key. If matching fails (entity added or removed),
     it is a shape change: matched objects are kept, the others are added or
     removed;
   - new content is copied into the old objects;
   - new → old references are **remapped** in the module's types and `texpr`
     (`TField`, `TTypeExpr`, `TInst`, type parameters…);
   - unmatched new objects become the new entities.
5. No `tclass` is ever recreated for a cached module anymore: `typeloadModule`
   takes an optional "previous module" and reuses its entities by key (through
   grafting).

Exit criterion: when fully retyping a modified module with grafting, the stale
pointer checker reports nothing across the whole corpus.

---

### Phase 3: fingerprints

1. `src/core/tFingerprint.ml`: canonical hasher (§3.2) for `t`,
   `tclass_field`, type shapes and `texpr`.
2. Compute `FieldSig` and `FieldInlineBody` at the end of each field's typing;
   `TypeShape` at the end of `PBuildClass` and `PConnectField`; `FieldBody` at
   the end of `PTypeField`.
3. **Per-field** `Decls` hash in `TypeloadParse`: split the AST per field and
   compute relative positions, which tells which fields changed without typing
   them.
4. Unit tests for the hasher: stability (two cold typings give the same
   fingerprint), sensitivity (every signature change changes the fingerprint),
   insensitivity to positions.

---

### Phase 4: dependency recording

Instrumentation points (all go through an `Incremental.record dep` that is a
no-op outside the server):

| Dependency | Where |
|---|---|
| `DLookup` | `Typeload.load_instance`/`load_type_def`, import resolution (`ImportHandling`, `typeloadModule.ml`), `TyperDotPath`, class path lookup (including **failed** lookups) |
| `DSig` | `FieldAccess`/`Fields` (field access), `Calls`, `CallUnification`; reading `cf_type` of other modules' fields |
| `DShape` | field lookup (`Fields.type_field`, `class_field`), `using`, structural unification (`tUnification.ml`, `TAnon` against `TInst`), `OverloadResolution`, `Matcher` (enum constructors), override and interface checks (`TypeloadCheck`), `AbstractCast` (`from`/`to`) |
| `DInline` | `Inline`, `Generic`, `MacroContext` (executed code) |
| `DMacro`, `DFile` | phase 6 |

The current consumer is carried by the typer context (`ctx.f.curfield` and its
module). Dependencies from a field to its own module are recorded too, since the
granularity is the field.

Optimization: dependencies are deduplicated per consumer, and `DShape` reads
within the own module are grouped.

Exit criterion: verification mode in "trace only" mode (everything is
recomputed, and compared with what the graph would have invalidated) shows that
**no node that changed would have been considered valid**. This is the test
that proves dependencies are complete.

---

### Phase 5: per-field validation and retyping (server core)

Replaces `check_module` and `type_module` (`serverCache.ml`).

1. **Red/green validation** on demand: `validate node` first validates its
   dependencies recursively and compares the fingerprints seen. If everything
   is green, the node is green; otherwise it is recomputed, and its new
   fingerprint is compared to the old one.
2. **Modified file**:
   1. reparse; compute `Decls` fingerprints per field and per type;
   2. added or removed types, and added or removed members: the `TypeShape`
      changes;
   3. fields whose `Decls` fingerprint changed: retype only those fields, in
      the existing entity (reset `cf_type` and `cf_expr`, rerun the field's
      typing, then graft);
   4. the type's build macros are re-executed if their `MacroExpansion` node is
      invalid. Their output is diffed per field as in 2.3.
3. **Early cutoff**: if `FieldSig(cf)` is unchanged after retyping, consumers of
   `DSig cf` stay green. Only `FieldBody(cf)`, then post-processing and
   generation of `cf`, are recomputed.
4. **On-demand module typing** (`Typeload` requesting a module): if the module
   is cached, validate the required `Shape` and `Sig` nodes rather than the
   whole module. Lazy field typing (`TLazy`) becomes "validate
   `FieldSig(cf)`".
5. **Display and completion**: the displayed file is always retyped (current
   `AllowPartialTyping`), but benefits from early cutoff for everything else.
6. **Errors and warnings**: stored per node and replayed when the node is
   green, replacing the current handling of modules with errors.
7. **Initialization macros** (`--macro`) and global callbacks: always executed,
   last, with early cutoff on what they produce (phase 6).

Exit criterion: equivalence across the whole corpus in history replay.
Measurement: number of retyped fields per commit, typing time.

---

### Phase 6: macros

1. **Expansion key**:
   - expression macros: `(called macro field, hash of AST arguments, consumer field, call index in the body)`;
   - build macros: `(built type, macro, hash of input fields)`.
2. **Tracing** in `src/macro/macroApi.ml`: every API function that reads the
   program records a dependency for the current expansion (`getType` gives
   `DLookup` + `DShape`, `typeof`/`typeExpr` the dependencies of the triggered
   typing, `getBuildFields` and `getLocalClass` the context, etc.).
3. **Interpreter sys primitives** (`src/macro/eval/evalStdLib.ml`):
   - `File.getContent`/`getBytes`, `FileSystem.exists`/`stat`/`readDirectory`:
     `DFile` (content, or sorted listing for a directory);
   - `Date.now`, `Math.random`, `Sys.getEnv`, `Sys.command`, `Sys.time`,
     sockets, file writes: the expansion is marked **volatile**.
4. **Static writes**: the eval interpreter instruments writes to a static field
   of a macro-context class **during** an expansion. The expansion is then
   marked volatile, and a `WMacroStaticState` warning is emitted ("this macro
   modifies static state: use the aggregation API to make it incremental").
5. **Recorded and replayed effects**: `defineType`, `defineModule`,
   `addMetadata`, `addResource`, `registerModuleDependency`, callbacks
   (`onGenerate`, `onAfterTyping`, `onTypeNotFound`) and messages are logged per
   expansion. When the expansion is green, the log is replayed instead of
   running the macro.
6. **Volatile macros**: re-executed on every compilation. If their output (and
   effect log) has the same fingerprint, downstream stays green.
7. **Macro code**: the expansion depends on `DInline` (body) of every macro
   context function it executes. Simple first version: depend on the bodies of
   all macro-context modules reached during execution, as recorded by the
   interpreter.
8. **Aggregation API** (new, `haxe.macro.Context` or `haxe.macro.Aggregate`):

   ```haxe
   // Contribution, cached with the node that emits it (a type, an expansion).
   Aggregate.contribute("hxbit.serializable", macro $v{cl.name});
   // Reduction: executed on every compilation over all contributions
   // (cached and new), with early cutoff on its result.
   Aggregate.reduce("hxbit.serializable", (all:Array<Expr>) -> defineRegistryType(all));
   ```

   Migrate hxbit (registry of `Serializable` classes), domkit (components) and
   Heaps (shader and resource registries) during this phase, to validate the
   API on real cases.

Exit criterion: Heaps, hxbit and domkit pass history replay with at most a
handful of identified and justified volatile macros.

---

### Phase 7: incremental filters

1. **Post-processed copy**: filters work on a copy of `cf_expr`
   (`PostProcessed(cf)`) and no longer mutate the typed AST.
   `save_class_state`, `cl_restore` and `cf_expr_unoptimized` go away: the
   typed AST remains the source and is used for subsequent inlining.
2. **Local filters**, which only depend on the field and on interfaces: cached
   by `PostProcessed(cf)`, depending on `FieldBody(cf)`, the `DSig`/`DShape`
   read and the platform configuration. This covers most of `run_safe_filters`
   and `destruction_on_scom`.
3. **Global passes, rewritten over per-field summaries**:
   - **DCE** (`src/optimization/dce.ml`): each post-processed field produces a
     summary "referenced fields and types, used features, `@:keep`".
     Reachability is a graph maintained incrementally: add or remove the edges
     of modified fields, then propagate the differences (mark or unmark).
     Acceptable first version: full reachability recomputation over summaries,
     very fast since no AST is walked.
   - **Descendants** (`add_descendant`): recomputed from shapes.
   - **Features** (`has_feature`): union of the features of reachable fields.
   - **`RenameVars`**: the global part (reserved names and collisions) is
     computed over summaries. Per-function renaming is local, hence cached.
   - **Purity (analyzer)**: fixed point computed over the call graph of
     summaries, per strongly connected component. Only touched components are
     recomputed.
4. Phase output: for each reachable field, a `PostProcessed` node, and a global
   set (kept types, features, descendants) with its fingerprint.

---

### Phase 8: JS generation

`src/generators/genjs.ml` is split into:

1. **Per-type fragment**: the JS code of a class, an enum or an implemented
   abstract, plus its metadata (static initialization dependencies, `__init__`,
   used features). Cached by `Fragment(js, type)`, depending on the
   `PostProcessed` nodes of its fields, referenced native names and the global
   set (features).
2. **Per-fragment source map**, with positions relative to the fragment.
3. **Linking**: header, type order (inheritance dependencies), static
   initialization, concatenation, source map offsets. Linear in output size,
   without walking any AST.
4. Only write the file if its content changed (to be shared with other
   targets).

Exit criterion: output byte-for-byte identical to cold mode (`tests/unit` on
JS, source maps included).

---

### Phase 9: HL generation

The heaviest part, because of the global tables in `genhl.ml` (`cglobals`,
`cstrings`, `cfunctions`, `cnatives`, `cached_types`, `anons_cache`,
`method_wrappers`…).

1. **Per-function compilation with symbolic references**: each function is
   compiled into a `fundecl` whose global operands are symbols
   (`SymString "foo"`, `SymGlobal path`, `SymFun (path, field)`,
   `SymType hl_type_key`) instead of indices.
2. **HL types**: a canonical, order-independent `hl_type_key` (today the type
   cache depends on encounter order). Class types and virtual objects
   (anonymous) are derived from `TypeShape` and `FieldSig`.
3. **Per-function `hlopt`**: optimization cached with the fragment (the result
   only depends on the symbolic function).
4. **HL linker**: assigns indices (types, globals, strings, functions, natives)
   in a **deterministic order** (sorted by key), resolves symbols and writes
   the bytecode and debug information. Linear in code size.
5. **HL/C (`hl2c`)**: generates one C file per type from the linked fragments,
   and only writes modified files (avoiding C recompilation).
6. Risk: index order changes the binary output compared to the current mode.
   A new deterministic format is accepted, and equivalence is checked between
   incremental and cold **after** the phase.

Exit criterion: `tests/unit` on HL (JIT and HL/C) passes, with identical output
between incremental and cold.

---

### Phase 10: cleanup

- Remove module-level invalidation: `DependencyDirty`, `m_checked`,
  `m_cache_state` (or reduce it to a diagnostic summary), `m_processed`.
- hxb: used only for on-disk persistence (server restart) and in verification
  mode. Loading from hxb must restore fingerprints and the graph.
- User documentation: new errors (field types), aggregation API, macro
  warnings, `incremental-verify` and `incremental-check-refs` defines.

## 5. Phase ordering and dependencies

```
Phase 0 ─┬─────────────────────────────────────────────┐
Phase 1 ─┤                                             │
         ├─ Phase 2 ─ Phase 3 ─ Phase 4 ─ Phase 5 ─┬─ Phase 7 ─┬─ Phase 8 (JS)
         │                                         │           └─ Phase 9 (HL)
         │                           Phase 6 ──────┘
         └─ (continuous verification on the whole corpus)      Phase 10
```

Phase 6 (macros) starts after phase 4 (same recording mechanism) and must be
complete before enabling phase 5 on Heaps, hxbit and domkit.

## 6. Risks and open points

- **Dependency completeness.** The main risk: a missing dependency yields a
  silently wrong result. Mitigation: the "trace only" mode of phase 4 and
  history replay in CI.
- **Memory cost** of the graph: estimated at a few tens of bytes per edge. To
  be measured in phase 4 on Heaps; if needed, compact per module (integer
  arrays indexed by entity).
- **Recording cost** during cold typing: must stay low. Disabled outside the
  server.
- **Structural unification**: `DShape` is coarse (adding a private field
  invalidates structural consumers). Possible refinement: separate public and
  private shapes.
- **Macros modifying other types** outside the logged effects (direct mutation
  through the `TypedExpr`/`ClassType` API of an object obtained via
  `getType`): detect it (write to an entity of another module during an
  expansion), and handle it with broad invalidation and a warning.
- **`@:multiType`, `@:genericBuild`, `onTypeNotFound`**: to be explicitly
  covered by tests.
- **Display**: check that completion is not slowed down by validation. If it
  is, validate only on demand.
- **Deterministic HL format**: a visible change for tools reading bytecode
  (debugger, profiler), to be validated with them.

## 7. Tests to add

- `tests/server`: one case per dependency kind (signature change, body change,
  inline body change, shape change, file shadowing a type, changed define,
  file read by a macro…), checking recomputed nodes through server statistics.
- `tests/misc`: phase 1 errors (non-inferred field types).
- `tests/incremental`: replay harness (phase 0) on `tests/unit`, Heaps, hxbit
  and domkit.
- Unit tests for the hasher and grafting (old ↔ new mapping, remapping, stale
  pointer checker).
