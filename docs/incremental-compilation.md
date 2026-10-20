# Incremental compilation: implementation plan

Goal: the compilation server only retypes, post-processes and regenerates what
actually changed, with a result **bit-for-bit identical** to a cold compilation.

## 0. Decisions

| Topic | Decision |
|---|---|
| Inference unit | **The field.** A field's type must be fully determined at the end of its own typing. A monomorph still open in a field's type (`static var x = []`) is an **error**: a type annotation is required. |
| Granularity | **The field**, directly: dependencies, fingerprints and retyping are per field. |
| Scope | Typing, filters and generation. |
| Targets | **JS and HL only.** Other targets (C++, JVM, Python, Lua, PHP, Neko, Flash) are **out of the scope of this plan**: they must keep compiling with a global generation step, but nothing in this plan makes their generation incremental. |
| Macros | Automatic tracing of compiler API reads; **volatility is explicit** (`Context.markVolatile()`), like file dependencies; volatile expansions are re-executed with early cutoff on their output. Console output (`Sys.println`, `trace`) is printed immediately, in no guaranteed order, and not replayed from the cache. **Macro statics are reset for every expansion**; `@:persistent` is reserved for initialization values (frozen afterwards); state that must outlive an expansion uses dedicated declarations (`MacroArray`, `MacroMap`, `MacroTypeData`, `MacroCache`, `MacroIndex`). File dependencies are **explicit**: only files declared with `registerModuleDependency` are tracked (and watched, wherever they are); reading or writing other files is the macro's responsibility. |
| Target release | **Haxe 6.** Work happens on the long-lived `haxe6-incremental` branch, regularly merging `development` in. New warnings (phase 1, cycles) may be backported to a Haxe 5.x release as a transition. |
| Compatibility | Breaking changes accepted for Haxe 6. The new rules are **warnings in Haxe 5.x** and **errors in Haxe 6, with no compatibility flag**. |
| Macro context and configurations | Every context on a server (the macro context, the target context, and other configurations such as js and hl, or debug and release) keeps a **separate query graph** (different defines, platform and standard library overrides), but they all **share parsing**: a parsed module is reused by another context when the defines read during its parsing have the same values in both (phase 3.6). |
| Static initialization order | Types are output and static vars initialized in **dependency (topological) order**, ties broken by path. All `__init__` blocks run in a **first pass**, before any static var initializer, in the same order. A dependency cycle between static initializers is an **error**. |
| Error reporting | **Configurable.** By default every independent error is reported: a failed query is marked failed, its dependents are skipped without reporting cascading errors, and typing continues. A flag stops after the first (or N) errors, in deterministic order. |
| Storage | **In memory only** at first, preserving the physical identity of objects, with no memory limit and no on-disk persistence: the graph size is measured before deciding on eviction or persistence. hxb is not on the critical path; it remains a verification tool and a candidate persistence format. |
| Positions | **Relative to an anchor** (one per field / type declaration). A field that only moved in its file is not retyped: updating its anchor fixes every position in its typed AST, cached errors, macro outputs and `haxe.PosInfos` (phase 2, item 6). |
| Display / LSP | **Deferred.** No new guarantees for editor requests in this plan; display keeps its current semantics, sped up by incrementality. To be designed once phase 6 is done. |
| Positions in inlined code | The inliner replaces expression positions with the call site's (`map_pos`, `src/optimization/inline.ml`), so they are excluded from fingerprints. Exceptions: `-D keep_inline_positions` (already part of the define signature; relative positions are then included in the body fingerprint), self-calling closures (relative positions included), and **`haxe.PosInfos` inside an inline body** (e.g. `trace`), which keeps the callee's position: with anchored positions (phase 2, item 6), a moved inline function changes its callers' output through its anchor, without changing its fingerprint (`tests/incremental/cases/positions/inline-positions.hxtest`). |
| Validation corpus | Repository tests (`tests/unit`, `tests/server`, `tests/display`); Heaps, hxbit, domkit and hide, for their heavy use of macros. |
| Typing order | **Fully demand-driven.** The typer passes (`PBuildModule` … `PFinal`) and the `delay`/`flush_pass` scheduler are removed. Typing is a set of queries (`declare`, `resolve`, `header`, `members`, `sig`, `body`, `checks`) that are the same nodes as the incremental graph. |
| Metadata on other types | A macro adding metadata to a type other than the one it builds is allowed, but that metadata is **only visible after typing** (`program()` stages: filters, DCE, generators), never to macros or typing (phase 7, item 6). |
| Macro-defined types | Owned by the **expansion that defines them** (`defineType`/`defineModule`): replayed and removed with it; a second definition of the same path elsewhere is an error. `onTypeNotFound` callbacks can only be registered by **initialization macros**. |
| Whole-program macro APIs | `Context.getAllModuleTypes()` and similar APIs are **only allowed in `program()` stages** (`onAfterTyping`, `onGenerate`); calling them during typing is an error. `Context.getModule(path)` stays allowed (it loads one specific module, a normal dependency). |
| Typing root | `program()` types **all fields of all loaded modules**, as today: errors in unused code are still reported. On-demand changes the order, not what gets typed. |
| Cycles | **Every query cycle is an error**, detected generically by the query engine, with no partial view, placeholder or retry. Main case: build macros (requesting the member set of a type whose build macro is running, directly or through another build macro). Only exception: a field referring to itself inside its own body. |
| Enforcing queries | Entity contents become **abstract types** behind accessors, so that the OCaml compiler rejects any read that bypasses a query. Post-typing stages use a frozen view. |
| Parallelism | **Every compilation operation runs in parallel**, not only typing: file system events and hashing, parsing, every typing query, macro expansions, fingerprints, post-processing, global passes, code generation and output writing (§4b). The query engine is the scheduler, on a persistent pool of OCaml 5 domains (`src/context/parallel.ml`). **Enabled by default** with one domain per available core; `-D compilation-domains=<n>` sets the number, `1` disables it. Output must be identical whatever the number of domains and the scheduling order (checked in CI). |
| File system | A server-wide **virtual file system** (phase 6) holds directory listings, file hashes and contents, kept up to date by a **file system watcher**, **editor notifications** (`server/invalidate`, overlays) and the compiler's own writes. Path resolution and file reads make **no system call** once an entry is known; a compilation works on a frozen snapshot. A full scan only happens at server startup, or for a directory the watcher cannot watch. |

## 1. Current state (starting point)

- The cache is **per module** (`module_def_extra`, `src/core/tType.ml`). Each
  dependency (`m_deps : (int, module_dep) PMap.t`) only records *which module*
  is depended upon, never *what* is used (interface, inline body, macro…).
- `check_module` (`src/compiler/server/serverCache.ml`) invalidates
  **transitively and blindly** (`DependencyDirty`): changing the body of a
  private function in a base module invalidates the whole graph above it.
- Change detection relies on modification time, or on the full parsed AST
  (`CheckFileContentModification`), which includes function bodies.
  Modification times are compared with a precision of **one second**
  (`sys_filetime` returns `st_mtime`, `libs/extc/extc_stubs.c`): a file edited
  twice within the same second is not seen as changed.
- Warm and cold compilations already differ in several situations, reproduced
  by `tests/incremental` (cases marked `@known-failure`): edits within the same
  second, a new file shadowing a loaded type from a higher priority class path,
  a new platform-specific file (`Impl.js.hx`) next to a loaded module,
  a monomorph bound by a previous compilation staying bound on a cached module,
  metadata added to another type by a macro staying on the cached type, types
  generated by `onTypeNotFound` not regenerated when the initialization macro
  changes, macro static
  state and `@:persistent` registries, and virtual file names of macro-defined
  modules built from a global counter.
- Open monomorphs are closed to `Any` when saving
  (`update_cache_dependencies ~close_monomorphs:true`, `src/filters/filters.ml`).
  Typing results therefore already depend on the cache state.
- Post-processing is global: `Filters.run` works on `com.types`. DCE,
  `RenameVars`, `add_descendant`, `has_feature` and the analyzer's purity
  inference all operate on the whole program. Filters mutate the typed AST
  **in place**, then `save_class_state` and `cl_restore` restore it.
- Typing is scheduled by passes (`src/context/typerPass.ml`): work is queued
  with `delay`/`delay_late` and run in pass order by `flush_pass`
  (`src/context/typecore.ml`). Typing is already partly lazy (`cl_build` with
  `Building`/`BuildMacro` states, `TLazy` field types), and `PForce` forces all
  remaining lazies. Many results depend on the order in which this happens.
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
6. **Demand-driven typing.** There are no global phases: every piece of typing
   is a query, computed when first demanded and memoized. The query engine is
   both the scheduler of a cold compilation and the validator of an incremental
   one.
7. **Parallel by default.** Every operation of a compilation is a task on the
   same scheduler (§4b); results never depend on the number of domains or on
   the scheduling order.

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
| `File(f)` | file content | content hash, from the virtual file system (phase 6). Changes are signaled by the file system watcher, editor notifications and the compiler's own writes; when a scan is needed, modification time (with full precision, `st_mtim`, not seconds) is only a pre-filter, and a file whose modification time is within the last second of its previous check is always hashed |
| `Decls(m)` | parsed AST, split per type and per field | hash of the AST with positions **relative to the declaration's anchor** (phase 2, item 6), per field |
| `TypeHeader(t)` | path, kind, parameters, constraints, `super`/`implements`, **declared** metadata, `@:using`, abstract underlying type, typedef target | canonical hash |
| `TypeMembers(t)` | **member set** after build macros (names, kinds, access, declared hints), complete metadata (including build-added), abstract `from`/`to`/operators | canonical hash |
| `FieldSig(cf)` | fully resolved type, kind (`var`/`method`, access), `cf_params`, metadata, relevant flags | canonical hash |
| `FieldInlineBody(cf)` | body of `inline`, `macro` and `@:generic` fields, and of abstract fields used as inline casts/operators | canonical hash of the `texpr` |
| `FieldBody(cf)` | typed `cf_expr` | canonical hash of the `texpr`, **including anchor-relative positions** (used by post-processing and generation, where positions matter) |
| `MacroExpansion(site)` | result of an expansion and its effects | hash of the output |
| `TypeLateMeta(t)` | metadata added to `t` by other types' macros, merged in expansion key order (phase 7, item 6) | canonical hash |
| `PostProcessed(cf)` | expression after filters | hash, including anchor-relative positions |
| `Fragment(t)` | generated code for a type or a function | hash |

**Canonical hasher** (new module `src/core/tFingerprint.ml`):
- module types are identified by path, never by address;
- type parameters by `(host, index)`;
- anonymous fields are sorted by name;
- bound monomorphs are followed; an unbound monomorph is forbidden in a
  signature, which phase 1 guarantees;
- there are no `TLazy` values left to force: with demand-driven typing
  (phase 3), hashing a signature demands the `sig` query;
- positions are excluded from interface fingerprints (`TypeHeader`,
  `TypeMembers`, `FieldSig`, `FieldInlineBody`, except in the cases listed in
  §0) and included, relative to their anchor, in `Decls`, `FieldBody`,
  `PostProcessed` and `Fragment`;
- it writes directly into an incremental digest, without intermediate
  serialization.

### 3.3 Dependencies

The *consumer* is the node currently being computed, usually the field being
typed (`ctx.f.curfield`), or a macro expansion. Dependency kinds:

| Kind | Recorded when | Invalidated if |
|---|---|---|
| `DHeader t` | reading the header of `t`: parameters, super class, interfaces, declared metadata (e.g. `is_parent`, unification against an interface, macro `ClassType` header access) | `TypeHeader(t)` changes |
| `DMembers t` | iterating over or looking up members of `t`, structural unification, `using`, matcher exhaustiveness, override checks, overload resolution, complete metadata after `Context.buildType` | `TypeMembers(t)` changes |
| `DSig cf` | reading `cf.cf_type` (field access, call) | `FieldSig(cf)` changes |
| `DInline cf` | inlining, `@:generic` instantiation, macro execution (macro context) | `FieldInlineBody(cf)` or `FieldBody(cf)` changes |
| `DLookup (path, result)` | resolving a type name, an import, an `import pack.*`, sub-types | the resolution result changes, including a file that **appears** and shadows another type (negative dependency) |
| `DMacro site` | using a macro expansion | `MacroExpansion(site)` changes |
| `DFile f` | a file declared with `Context.registerModuleDependency` (the only way a macro declares a file dependency: reads are not tracked automatically) | `File(f)` changes |
| `DDefine` | already covered by the define signature (`m_sign`) | |

Storage: `m_deps` stays as a module-level summary, for existing tools and
diagnostics. The new graph lives in a separate structure, `IncrementalGraph`,
attached to the cache context (`CompilationCache`):

```ocaml
type node_key =
  | NFile of Path.UniqueKey.t
  | NDecls of path                       (* module *)
  | NHeader of entity_key
  | NMembers of entity_key
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
   - the fingerprints of all `Header`, `Members`, `Sig` and `Body` nodes;
   - the generated outputs, byte for byte;
   - the sorted list of errors and warnings.
2. **Stale pointer checker** `-D incremental-check-refs`: walk all cached
   modules and check that every reference to an entity of module `X` (in
   types, `texpr`, `cl_super`, `a_from_field`…) points to an object reachable
   from `X.m_types`.
3. **Scenario suite** (`tests/incremental/`, already in place): `.hxtest`
   scenarios of edit steps, each step compiled warm (server) and cold and
   compared (exit code, messages, program output, generated files), plus
   expectations per step. About 100 cases over basic edits, positions,
   inference, type hierarchies, inline, `@:generic`, macros, static
   initialization and DCE, on `interp` and `js`. Bugs of the current server and
   Haxe 6 semantics (`cases/haxe6/`) are marked `@known-failure`; the runner
   flags them as soon as they pass. Each phase adds cases for what it changes
   and removes the markers it fixes.
4. **History replay tool** (`tests/incremental/src/Replay.hx`, in place): a Haxe harness that starts a
   server, takes a repository and a list of commits, compiles each commit
   incrementally in verification mode, and produces a report (timings,
   recomputed nodes, differences).
   - Corpus: `tests/unit`, `tests/server`, `tests/display`; Heaps, hxbit,
     domkit and hide (repositories cloned in CI, with replayed commits).
5. **Statistics** (`--times` and `server/stats`): nodes validated, recomputed
   and cut off by early cutoff, per node kind.

Exit criterion: the harness runs in CI against the current server
(module granularity) and reports no difference. This gives a timing baseline.

**Status.** Items 3, 4 and 5 are in place (scenario suite in CI, history
replay, VFS statistics through `server/vfs` and `-v`). Item 1 (in-compiler
verification mode) is covered by the two external harnesses until fingerprints
exist (phase 4); item 2 (stale pointer checker) only makes sense with grafting
(phase 2) and is built with it.

**First replay** (last 30 commits of Heaps, compiled to JS with `include` of
`h3d`, `h2d`, `hxsl`, `hxd`, with hxbit, domkit, castle and format):
- 29 of 30 commits produce a JS file **identical in content but not in type
  order** between the warm server and a cold compilation; the same happens
  with the current nightly compiler, so this is pre-existing: the output order
  of the JS generator depends on the order in which modules are typed or
  restored from the cache. This is the deterministic output order required by
  the plan (§0, static initialization and output order; §3.4);
- with `--ignore-order`, all 30 commits are identical;
- warm compilations take 82 s in total against 181 s cold (module-level
  caching of the current server).

---

### Phase 1: semantic rules (Haxe 6 breaking change)

Goal: make inference local to the field.

1. **Detection.** At the end of a field's typing (after resolving the `TLazy`
   set by `typeloadFields.ml`, when `cf_type` is final), walk `cf_type` looking
   for an unbound `TMono` or an open `TAnon` (mutable `a_status`).
   - Phase 1a: warning `WUnresolvedFieldType`: "The type of `x` is not fully
     inferred, add a type annotation".
   - Phase 1b (Haxe 6): error, with no compatibility flag.
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
- Cycles of unannotated fields (`f` calls `g`, `g` calls `f`): today they may
  "work" through `LProcessing` placeholders depending on order. They become an
  error asking for an annotation once phase 3 lands (§ phase 3.3); phase 1
  only warns about them. A field referring to itself in its own body stays
  allowed.
- Locals and closures: not affected, they are internal to the body.

Consequence: type parameter constraints no longer need a late pass
(`PCheckConstraint`, `delay_if_mono`). Constraints on a field's signature are
checked when the field's typing ends, and constraints on local types when the
body's typing ends, since no monomorph can be bound later.

Exit criterion: the whole CI is green with the error enabled, and the impact on
the corpus is documented.

**Status.** `WUnresolvedFieldType` is implemented
(`TypeBinding.check_resolved_field_type`, `src/typing/typeloadFields.ml`),
disabled by default (`-w +WUnresolvedFieldType` to enable), with a test in
`tests/misc/eval/projects/unresolvedFieldType`. The binding instrumentation
(item 2) is not needed for the measurement: a monomorph bound by another field
is necessarily still open at the end of its own field's typing, and is reported
there. Cross-field cycles resolved through `LProcessing` placeholders are
measured with the phase 3 cycle instrumentation.

**Measurements** (`-w +WUnresolvedFieldType`):

| Corpus | Fields reported |
|---|---|
| `tests/unit` (macro) and the standard library it uses | 33 (5 in `std`) |
| Heaps, hxbit, domkit, castle, compiled to js (`include` of `h3d`, `h2d`, `hxsl`, `hxd`, `hxbit`) | 43 (5 in `std`) |

What is reported, by kind:
- **static caches initialized to `null`, `[]` or `new Map()`** and filled
  later (`hxsl.Checker.GLOBALS`, `hxsl.Serializer.TVECS`,
  `hxbit.Serializer.CL_BYID`/`CLIDS`, `domkit.Identifier._CACHE`,
  `haxe.Serializer.BASE64_CODES`, `haxe.Unserializer.CODES`…): about half of
  the cases;
- **member fields** of the same kind (`h3d.scene.Scene.tmpPasses`,
  `prevEngine`);
- **untyped function arguments** that the body does not constrain
  (`h3d.scene.CameraController.fov(delta)`, `hxd.res.NanoJpeg.syntax(flag)`,
  `hxsl.HlslOut.handleMeta`…): the argument stays a monomorph, today bound by
  the first caller;
- **return types with an unbound type parameter**:
  `haxe.coro.SuspensionResult.withError` returns `SuspensionResult<Unknown>`,
  and `cdb.Types.toGuid` returns `Guid<Unknown>`. Since the field type is
  shared, the first caller binds it for every other caller: a latent bug today,
  an error in Haxe 6 (fixed with a type parameter or an annotation).

Every case is fixed with a type annotation. Fixing the standard library ones is
part of this phase.


---

### Phase 2: stable identity and content separation

Goal: be able to mutate an entity in place without breaking other modules'
pointers.

1. **Entity keys** (§3.1) and a per-module `entity_key -> object` index in
   `CompilationCache`.
2. **Entity positions** (`cf_pos`, `cf_name_pos`, `ef_pos`, `mt_pos`/`cl_pos`,
   `e_pos`, `t_pos`, `a_pos`, `*_name_pos` in `src/core/tType.ml`) carry
   anchors (item 6), so a moved declaration only needs its anchor updated;
   they can stay immutable. Grafting copies them when a declaration is
   retyped. `ef_index` stays immutable: an index change is a shape change.
3. **External contributions: move them out of entities, or make them
   recomputable.**
   - `cl_descendants`: recomputed by the aggregation pass (phase 8) from the
     `cl_super` and `cl_implements` of all types; no longer stored
     persistently.
   - DCE and post-processing flags (`CfUsed`, `CfPostProcessed`, …): moved to
     post-processing data (phase 8), out of `cf_flags`.
   - `m_features`: computed per module from its post-processed AST.
   - Metadata added to a type by a macro other than that type's own build
     macros (`Compiler.addMetadata`, `meta.add` on a type obtained through
     `getType`): stored separately as *late metadata*, only visible after
     typing (phase 7, item 6). Initialization macros (`--macro`, including
     `addGlobalMetadata`) are fine: their effect is part of the declared
     metadata.
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
   grafting). After phase 3, this becomes the behavior of the `declare` query.
6. **Relative positions.** Without them, a field that did not change but moved
   in its file (lines added above it) keeps stale absolute positions in its
   typed AST, since `Decls` fingerprints ignore absolute positions and the
   field is not retyped. This breaks source maps and debug information, cached
   errors and warnings, positions in macro outputs, and line numbers baked
   into the code (`trace`, `haxe.PosInfos`, which today is a `TConst (TInt line)`
   in the typed AST, read back by generators such as `genjs.ml`).
   - `pos` (`src/core/globals.ml`, `{ pfile; pmin; pmax }`) gets an
     **anchor**: a shared mutable object `{ mutable delta : int }`, one per
     field, per type declaration header and per module top level. The absolute
     offset is `pmin + anchor.delta`.
   - The parser allocates one anchor per declaration and creates every
     position inside it with that anchor. Positions created while typing a
     field (generated expressions, macro outputs through
     `Context.currentPos()`, inlined code taking the call site position) use
     the anchor of the position they derive from.
   - On reparse (phase 6), each unchanged declaration is matched (by `Decls`
     fingerprint, phase 4, which uses positions relative to the anchor) and its
     shift is applied to its anchor: O(1) per declaration, and every position
     of its typed AST, cached messages and macro expansions is correct again.
   - `pos` becomes an abstract type: the ~350 direct reads of `pmin`/`pmax`
     in `src/` go through `Pos.min`/`Pos.max`. Structural comparison and
     hashing of positions (`p1 = p2`, hashtables keyed by `pos` in display
     and statistics) use absolute values.
   - Line and column numbers stay computed lazily from absolute offsets.
     `haxe.PosInfos` is no longer baked as an integer constant during typing:
     it becomes a position-carrying node resolved to `lineNumber` during
     post-processing (phase 8), from the anchored position.
   - Positions pointing into another file (macro reification positions in the
     macro's module, `-D keep_inline_positions`) carry that file's anchors, so
     they follow edits of that file as well.
   - hxb serializes positions relative to their anchor.
   - Principle 4 still holds: the typed AST is not modified, only the anchors
     it refers to.

Exit criterion: when fully retyping a modified module with grafting, the stale
pointer checker reports nothing across the whole corpus.

---

### Phase 3: demand-driven typing

Goal: replace the pass scheduler with queries. This is the largest refactor of
the plan, and the foundation of phases 4 to 6: the queries defined here are the
nodes of the incremental graph, and dependency recording (phase 5) happens
inside their accessors.

#### 3.1 Queries

The key idea is to **separate declaring an entity from defining its contents**.
An entity is created empty by `declare` and can be referenced by pointer
(`TInst(c, …)`) immediately; only *inspecting* its contents demands work.

| Query | Computes | Demands | Replaces |
|---|---|---|---|
| `declare(module)` | parse, create empty entities with their stable keys | `File` | `PBuildModule` |
| `resolve(name, scope)` | type name resolution (imports, `using`, sub-types, class paths) | `declare` of candidates | lookups spread over `Typeload`/`ImportHandling` |
| `header(T)` | type parameters, constraints, super class, interfaces, declared metadata, abstract underlying type, typedef target | `resolve` of each hint | first half of `PBuildClass` |
| `members(T)` | member set (names, kinds, access, declared hints) **after build macros**, complete metadata, properties connected to accessors, abstract `from`/`to`/operators | `header(T)`, `members` of ancestors (`@:autoBuild`), build macro expansions | rest of `PBuildClass`, `PConnectField`, `cl_build` |
| `sig(f)` | fully resolved field type, constraints checked | hints; for a function whose body has no `return <expr>`, a syntactic `Void` return type; otherwise `body(f)` for an inferred type | `TLazy` on `cf_type` |
| `body(f)` | typed expression, local constraints checked | `sig`/`members` of whatever it touches | `PTypeField` |
| `checks(T)` | overrides, interface implementation, abstract classes, final vars, overloads, core API, abstract class constraints | `members(T)`, `members(ancestors)`, `sig` of involved fields | `PForce` checks, `delay_late` ordering |
| `program()` | root driver: `body` and `checks` of every field and type of every loaded module, then the post-typing callbacks | everything | `PForce`, `PFinal` |

Every query has a **state**: `NotStarted | Running | Done | Failed`. Demanding a
query that is `Running` is a cycle (§3.3).

`program()` keeps today's semantics: all fields of every loaded module are
typed, so errors in unused code are still reported. Loading a module (through
`resolve`) adds it to the set `program()` will process, until a fixed point.

#### 3.2 Enforcing queries with abstract types

All direct reads of entity contents are replaced by accessors. Today `src/`
contains about 1,400 such reads (`cf_type` 423, `cl_super` 186,
`cl_ordered_fields` 139, `cl_ordered_statics` 127, `cl_fields` 117,
`cl_statics` 112, `cl_constructor` 110, `cl_implements` 86, `e_constrs` 58…).

1. `tclass`, `tclass_field`, `tenum`, `tenum_field`, `tabstract` and `tdef`
   become abstract in a module signature (`src/core/tEntity.mli` or similar).
   Contents are only reachable through accessors grouped by query:
   `Header.super c`, `Header.meta c`, `Members.fields c`, `Sig.field_type cf`,
   `Body.expr cf`…
2. Each accessor demands the query (computing it if needed) and records the
   dependency for the current consumer (phase 5).
3. Writers are restricted to the query implementations (typeload modules) and
   to grafting (phase 2).
4. **Frozen view**: once `program()` is done, filters, analyzer and generators
   use a read-only view without demand or recording overhead (still through
   abstract types, so that post-typing code cannot mutate typed entities —
   principle 4).
5. Migration is mechanical but large: done module by module, with a temporary
   escape hatch (`Unsafe.cl_fields`) whose uses are tracked and must reach zero.

#### 3.3 Cycles

**Rule: every cycle is an error.** The query engine detects cycles generically:
demanding a query that is `Running` on the same query stack raises a cycle
error. No query ever returns a partial or placeholder result, and nothing is
retried later in the hope that the cycle resolves itself.

This removes the current order-dependent mechanisms:

- `Build_canceled` and the rebuild loop (`typeloadModule.ml`,
  `typeloadCheck.ml`): today a class whose super class is still building
  cancels its build and retries in a late `PBuildClass` task, and an error is
  only raised when no progress is made ("Loop in class building prevent
  compiler termination"). With queries, `members(Child)` simply demands
  `members(Super)` first; a real cycle is reported immediately.
- `BuildMacro` pending lists and the `Building` state in `cl_build`.
- `LProcessing` placeholders (`make_lazy`, `src/context/typecore.ml`): today a
  field whose type is being inferred exposes its placeholder monomorph to
  whoever demands it, which allows cross-field inference cycles to "work"
  depending on order.

What is and is not a cycle:

- **Declarations** never cycle: `class A extends B<A>` only needs `declare(A)`,
  not `header(A)`.
- **Headers**: `header(A)` demanding `header(A)` (`class A extends A`, typedef
  loops) is an error, as today.
- **Build macros** (the main case):
  - A's build macro reads B's members (`getType("B")` then fields,
    `typeExpr` of code touching B's fields…) while B's build macro reads A's
    members: error.
  - A's build macro types an expression that accesses a field of A itself
    (e.g. `typeExpr(macro A.foo)`): error, because it demands `members(A)`
    while it is running.
  - `@:autoBuild` on a super class whose macro reads the members of the
    subclass being built: error.
  - Inside its own build macro, a type exposes its `header` (path,
    parameters, super class, interfaces) and its metadata as being built
    (declared metadata plus what the running build macros added so far,
    readable and writable) through `getLocalClass()`. Its fields are only
    available through `Context.getBuildFields()`; reading
    `getLocalClass().get().fields` is an error instead of returning a partial
    list.
  - Reading the members of the super class or of interfaces is not a cycle:
    it is a regular dependency (`members(Super)` is completed first).
- **Inferred signatures**: `sig(f)` demanding `sig(g)` demanding `sig(f)`
  (mutually recursive fields without annotations) is an error asking for an
  annotation on one of them. A function whose body contains no
  `return <expr>` (nested functions excluded) has a **syntactic `Void` return
  type**: its `sig` only depends on its argument hints, never on its body, so
  it takes no part in such cycles, and editing its body never changes its
  signature. The only exception is a field referring to itself
  inside its own body (`function fact(n) return n <= 1 ? 1 : n * fact(n - 1)`):
  this is not a query re-entry but the same inference unit, so the body sees
  its own signature monomorph, which must be closed when the field's typing
  ends (phase 1).
- **Generic instances, abstracts, typedefs**: same rule, through `header` and
  `members` of the instance or underlying type.

**Metadata is not split, and reading it does not trigger a build.** A type's
metadata is a single set: declared metadata plus metadata added by its build
macros. A macro can read another type's metadata without building it, or force
the build first when it needs the metadata the build adds.

For incremental compilation, a read must give the same answer whatever the
order in which types happen to be built. "The metadata as it is right now"
would differ between a cold compilation (the other type not built yet) and a
warm one (built in a previous run). So the view a macro gets depends only on
what *that macro* demanded:

- **Without forcing the build**: the declared metadata (`header(T)`), even if
  the type has already been built for another reason. Never a cycle.
- **After forcing the build** (`Context.buildType(t)` in the same expansion):
  the complete metadata, including what the build added (`members(T)`).
  A type still building on the current query stack is a cycle error.

The macro API's `ClassType` (and `EnumType`, `AbstractType`) becomes lazy so
that header information never triggers a build:

| Access | Demands |
|---|---|
| `Ref.get()`, `Ref.toString()`, `name`, `pack`, `module`, `pos`, `params`, `superClass`, `interfaces`, `kind`, `isInterface`, `isExtern`… | `header(T)` only, never a build |
| `meta.get()`, `meta.has()`, `meta.extract()` | `header(T)`: declared metadata; complete metadata if the expansion forced the build |
| `Context.buildType(t)` / `ClassType.ensureBuilt()` | `members(T)`: triggers and completes the build |
| `fields.get()`, `statics.get()`, `constructor.get()`, `init` | `members(T)` (they need the build output anyway) |
| `cf.type` of a field, `overrides` | `members(T)` and `sig(f)` |
| `cf.expr()` (typed body of another field) | `body(f)`: triggers the typing of the body and records a dependency on it; reading the body of a field being typed on the current query stack (its own, or through a cycle) is a cycle error |

Behavior change: today `Ref.get()` always forces the build
(`encode_tclass` calls `cl_build` in `src/macro/macroApi.ml`) and tolerates a
type still building by returning its partial state. Code that relies on
`get()` to force a build before reading build-added metadata must call
`Context.buildType` instead:

- hxsl `loadShader` (`heaps/hxsl/Macros.hx`) reads the `:src` metadata added
  by the imported shader's build;
- hxbit reads `:isProxy` (added by a struct's build) on field types;
- domkit's `c.get(); // force build` before reading its components registry.

Reading only declared metadata needs no change and never cycles: hxbit's
`isSerializable` check of `:isSerializable` on `Inventory` while building
`Player` (and the reverse) keeps working.

To help migration, `-D macro-meta-check` warns when a macro reads the
metadata of a type that has build macros (`@:build`, or `@:autoBuild` on an
ancestor or interface, known from its header) without having forced its
build: "metadata of `T` read before its build; call
`Context.buildType` if build-added metadata is needed".

The cycle error shows the full query stack, for example:

```
Cycle detected while building types:
  members(game.Hero)      build macro game.Macros.build (Hero.hx:3)
  members(game.Inventory) build macro game.Macros.build (Inventory.hx:5)
  members(game.Hero)      <- cycle
```

**Measuring before switching.** Early in phase 3, the current compiler is
instrumented to detect these situations (a build cancellation, a read of a
`Building` class, a `LProcessing` placeholder demanded from another field) and
report them as warnings. Running it on the corpus gives the list of code to fix
before cycles become errors.

**Status.** Implemented as the `WTypingOrder` warning, disabled by default
(`-w +WTypingOrder`), with a test in `tests/misc/eval/projects/typingOrder`:
build retries (`Build_canceled`, `typeloadModule.ml`), a macro reading a class
whose build is not finished other than the class being built
(`MacroApi.partially_built_class_read_hook`, set in `macroContext.ml`), and a
field reading the placeholder type of another field whose type is being
inferred (`TFunctions.lazy_processing_read_hook`,
`TypeBinding.fields_being_inferred` in `typeloadFields.ml`; annotated fields and
self recursion are not reported).

**Measurements** (`-w +WTypingOrder`, distinct reader/read pairs):

| Corpus | Inference cycles between fields | Build retries / partial class reads |
|---|---|---|
| `tests/unit` (macro), standard library, utest | 27 (22 in `std`, 18 of them in `haxe.macro.Printer`) | 0 |
| Heaps, hxbit, domkit, castle (js, without `h2d.domkit`) | 53 | 0 |

Of these 66 distinct cycles, 27 go through a function that never returns a
value. With the syntactic `Void` rule (decided, §3.3, and applied by the
instrumentation), **39 cycles remain** (22 in `tests/unit`, the standard library
and utest; 29 in Heaps, hxbit, domkit and castle, some shared), each fixed with
one annotation.


#### 3.4 Determinism

- Query results must not depend on the order in which queries are demanded.
- Errors and warnings are stored per query and reported sorted by position.
- A query that raises an error is marked `Failed`. Queries demanding a failed
  query fail too, without reporting their own cascading error. `program()`
  keeps going over independent queries. `-D max-errors=<n>` (or a CLI flag)
  stops early: `program()` stops scheduling new queries once `n` errors are
  found, finishes the running ones, and reports errors sorted by position.
  With parallel typing the exact set of errors found before stopping may
  vary; the default (report everything) is fully deterministic.
- `com.types`, `m_deps` and any list built during typing are sorted by stable
  key before use.
- Display requests keep their current semantics for now (display is
  deferred, see decisions).

- Other sources of order dependence found in the compiler, fixed as part of
  this phase:
  - **Stored typed expressions** (`Context.storeTypedExpr`, `storeExpr`, and
    the internal `push_this`) get their id from a global counter
    (`store_typed_expr`, `src/context/typecore.ml`), and the id ends up in the
    expansion output (`@:storedTypedExpr N`). Ids become local to the
    expansion key: `(expansion key, n)`.
  - **Virtual file names** of types and modules defined by macros contain a
    global counter (`generate_virtual`, `src/context/common.ml`). They are
    derived from the defined module path instead, which is unique.
  - **`Compiler.include`** iterates over `readDirectory` without sorting
    (`std/haxe/macro/Compiler.hx`): the listing is sorted.
  - **Error messages**: monomorphs are printed with their global id
    (`Unknown<12>`, `src/core/tPrinting.ml`) and "did you mean" suggestions
    depend on the modules loaded so far. Monomorphs are numbered locally to
    each message (`Unknown<0>`, `Unknown<1>`…), and suggestions are computed
    from the class path rather than from loaded modules.
  - **`Hashtbl` iteration** over modules or types in the typer, filters and
    the JS and HL generators is replaced by iteration in stable key order.
  - **Anonymous structure naming** (`Tanon_identification`, used by hxb)
    assigns names in encounter order. Names are derived from a hash of the
    structure, with collisions ordered by stable key.

#### 3.5 Parallel typing

All typing queries (`declare`, `resolve`, `header`, `members`, `sig`, `body`,
`checks`) run on the parallel scheduler described in §4b, which also covers
the rest of the compilation. For typing specifically:

- `body(f)` queries are independent once their signatures are available, and
  are the bulk of the work; `header`, `members` and `sig` queries of
  independent types also run in parallel.
- Shared mutable state of the typer is turned into queries or protected:
  - module loading (`declare`) and the module table;
  - `@:generic` instance creation (query keyed by generic and type arguments);
  - `a_status` of anonymous structures (closed per phase 1, so read-only after
    the defining field is done);
  - counters used for naming (generated locals, anonymous types) become
    per-query and deterministic.

#### 3.6 Parsing shared between contexts

Every context on a server keeps its own query graph: the macro context
(`com.get_macros()`) and the target context, but also other configurations
compiled by the same server (js and hl, debug and release…). They have
different defines, possibly a different platform, and resolve different
standard library overrides (`std/<platform>/_std`), so their headers, members
and bodies cannot be shared. Parsing, however, is the same work in all of them
whenever conditional compilation evaluates the same way.

1. **Recording define reads.** While parsing a file, the preprocessor
   (`src/syntax/parserEntry.ml`, evaluation of `#if` conditions through
   `Define.raw_defined_value`, and the `macro` define read when computing
   `in_macro`) records every define it reads, with the value it saw (or
   "undefined"). Short-circuit evaluation is fine: only defines actually read
   can influence the result.
2. **Parse cache key.** A parse result is stored under the file's content hash,
   with its set of `(define, value)` reads. A context can reuse it if every
   recorded define has the same value in that context. Files without any `#if`
   are shared unconditionally.
3. Several parse results may coexist for one file (one per distinct set of
   values), for example one for macro and one for the target when the file
   contains `#if macro`.
4. `Decls` fingerprints (phase 4) are attached to the parse result, so they are
   shared as well.
5. This also replaces the current per-signature parse cache (`c_decls` in
   `CompilationCache`), which is keyed by the full define signature and
   therefore never shared between contexts with different defines.

Exit criterion: the pass scheduler is removed (`typerPass.ml`, `delay`,
`flush_pass`, `delay_if_mono`, `make_lazy`'s forcing, `build_state`), all
tests pass, compilation output is unchanged compared with the pass-based
compiler on the whole corpus (modulo the documented phase 1 and cycle errors),
and parallel typing produces the same output as sequential typing.

---

### Phase 4: fingerprints

1. `src/core/tFingerprint.ml`: canonical hasher (§3.2) for `t`,
   `tclass_field`, type shapes and `texpr`.
2. Compute fingerprints when the corresponding query completes: `TypeHeader`
   at the end of `header(T)`, `TypeMembers` at the end of `members(T)`,
   `FieldSig` and `FieldInlineBody` at the end of `sig(f)`, `FieldBody` at the
   end of `body(f)`.
3. **Per-field** `Decls` hash in `TypeloadParse`: split the AST per field and
   compute anchor-relative positions, which tells which fields changed without
   typing them.
4. Unit tests for the hasher: stability (two cold typings give the same
   fingerprint), sensitivity (every signature change changes the fingerprint),
   insensitivity to positions.

---

### Phase 5: dependency recording

Most dependencies are recorded inside the phase 3 accessors: `Header.*`
records `DHeader`, `Members.*` records `DMembers`, `Sig.*` records `DSig`, `Body.*` records `DInline`,
and `resolve` records `DLookup`. The table below lists where each kind
originates, which is also the checklist for the accessor migration.
Recording goes through an `Incremental.record dep` that is a no-op outside the
server:

| Dependency | Where |
|---|---|
| `DLookup` | `Typeload.load_instance`/`load_type_def`, import resolution (`ImportHandling`, `typeloadModule.ml`), `TyperDotPath`, class path lookup (including **failed** lookups) |
| `DSig` | `FieldAccess`/`Fields` (field access), `Calls`, `CallUnification`; reading `cf_type` of other modules' fields |
| `DHeader` | `is_parent`/`extends` checks, unification against interfaces, type parameter constraints, macro `ClassType` header access and declared metadata reads |
| `DMembers` | field lookup (`Fields.type_field`, `class_field`), `using`, structural unification (`tUnification.ml`, `TAnon` against `TInst`), `OverloadResolution`, `Matcher` (enum constructors), override and interface checks (`TypeloadCheck`), `AbstractCast` (`from`/`to`) |
| `DInline` | `Inline`, `Generic`, `MacroContext` (executed code) |
| `DMacro`, `DFile` | phase 7 |

The current consumer is carried by the typer context (`ctx.f.curfield` and its
module). Dependencies from a field to its own module are recorded too, since the
granularity is the field.

Optimization: dependencies are deduplicated per consumer, and `DMembers` reads
within the own module are grouped.

Exit criterion: verification mode in "trace only" mode (everything is
recomputed, and compared with what the graph would have invalidated) shows that
**no node that changed would have been considered valid**. This is the test
that proves dependencies are complete.

---

### Phase 6: per-field validation and retyping (server core)

Replaces `check_module` and `type_module` (`serverCache.ml`).

1. **Red/green validation** on demand: `validate node` first validates its
   dependencies recursively and compares the fingerprints seen. If everything
   is green, the node is green; otherwise it is recomputed, and its new
   fingerprint is compared to the old one.
2. **Modified file**:
   1. reparse; compute `Decls` fingerprints per field and per type;
   2. unchanged declarations that moved get their anchor updated (phase 2,
      item 6);
   3. added or removed types, and added or removed members: `TypeMembers`
      changes (and `TypeHeader` if the header changed);
   4. fields whose `Decls` fingerprint changed: their `sig` and `body` queries
      are invalidated and recomputed in the existing entity, then grafted;
   5. the type's build macros are re-executed if their `MacroExpansion` node is
      invalid. Their output is diffed per field as in 2.4.
3. **Early cutoff**: if `FieldSig(cf)` is unchanged after retyping, consumers of
   `DSig cf` stay green. Only `FieldBody(cf)`, then post-processing and
   generation of `cf`, are recomputed.
4. **Same queries, cold or warm**: the phase 3 queries check the graph before
   computing. If a cached node can be validated green, its stored result is
   used; otherwise the query recomputes it. There is no separate "cached
   module" path anymore.
5. **Display and completion**: deferred (see decisions). In the meantime the
   current semantics are kept: the displayed file is always retyped (current
   `AllowPartialTyping`), and it benefits from early cutoff for everything
   else.
6. **Errors and warnings**: stored per node and replayed when the node is
   green, replacing the current handling of modules with errors.
7. **Initialization macros** (`--macro`) are always executed **first**; the
   hash of the `@:persistent` values they produce acts like a define (phase 7,
   item 4). **Global callbacks** (`onAfterTyping`, `onGenerate`) and
   `program()` stage reductions are always executed **last**, with early
   cutoff on what they produce.

8. **File change detection and virtual file system**: see below. The server
   never scans the project on each compilation, and resolving a path never
   makes a system call.

#### Virtual file system

**Status (first version, `src/core/vfs.ml`).** Implemented:
- server-wide cache of file information (kind, full precision modification
  time, size) and sorted directory listings, keyed by absolute paths;
- libuv `fs_event` watchers on every directory the compiler reads, started at
  the beginning of the next compilation (libuv is used from the compilation
  thread only); entries read before their directory was watched are re-read
  once the watch is active; directories that cannot be watched are
  revalidated on every compilation;
- events applied at the start of each compilation (`Vfs.begin_compilation`),
  then a stable view; `server/invalidate` also invalidates the VFS;
- the current directory tracked by the VFS (`Vfs.chdir`, used by every
  directory change of the compiler and of the eval `Sys.setCwd`), so that
  relative paths do not cost a `getcwd`;
- users: `file_time` (module and parse cache times, now with full precision),
  class path directory listings and absolute path lookups;
- module checks in the server re-resolve the module path through the class
  paths (`check_module_resolution` in `serverCache.ml`) instead of looking for
  shadowing files in changed directories: this detects files shadowing a
  module from a class path with a higher priority and new platform-specific
  files;
- `File system calls (VFS)` in the server statistics (`-v`), and
  `HAXE_VFS_TRACE=1` to log every call; `HAXE_VFS=uncached` disables caching
  across compilations.

Results: the four change detection bugs of the current server reproduced by
`tests/incremental` are fixed (edits within the same second, two shadowing
cases, platform-specific file), and the whole suite passes with real
modification times (`--real-mtime`). On a small project, compilations
without changes make **0** file system calls from the fourth compilation on
(the first ones re-read entries as watches start). `tests/unit` and
`tests/server` pass.

Second round:
- **writes**: `Vfs.write_file` writes a file only if its content changed (digest
  of the previous write, or comparison with the file when unknown) and keeps
  the VFS up to date; used for HL bytecode and every HL/C file (unchanged C
  files are not rewritten). Files and directories written, deleted or renamed
  by macros (eval `sys.io.File`, `sys.FileSystem`) and directories created by
  the compiler (`Path.mkdir_from_path`) invalidate the VFS immediately, so that
  a module generated by a macro is visible in the same compilation
  (`tests/incremental/cases/vfs/macro-writes-module.hxtest`);
- **overlays**: `server/overlay` (`file`, optional `contents`) sets or removes
  the unsaved content of a file; module sources are read through
  `Vfs.read_file`; an overlay can add a file that does not exist on disk yet
  (`cases/vfs/overlays.hxtest`, where the cold compilation sees the overlay
  content on disk);
- **remaining direct accesses moved to the VFS**: `import.hx` lookup, std class
  path detection, display file check, display toplevel and path exploration,
  `server/setCwd`, `Context.includeFile`, hxb libraries, shadowing check of
  extern modules, reserved Windows file names, debug paths of HL;
- **measurement in tests**: `server/vfs` returns the number of file system
  calls of the previous request; `@expect-no-fs-calls` compiles the warm
  project again without changes until the watches are active and checks that
  the last compilation makes no file system call
  (`cases/vfs/no-fs-calls.hxtest`, across edits, a new directory shadowing a
  library type, and deletions).

Not done yet: content hashes (a file system with a one second modification
time precision still misses two edits within the same second), a resolution
index kept across compilations (class path lookup caches are rebuilt per
compilation, on top of VFS listings, without system calls), the eval read API
(`exists`, `readDirectory`…) which stays on the disk by design (explicit file
dependencies), native libraries (`nativeLibraryHandler.ml`), and the output
of the JS generator (streamed to the file, not through `write_file`).

Today, every compilation creates new class path objects, so the directory
listings (`readdir_cache` in `src/core/classPath.ml`) and resolved paths
(`file_lookup_cache` in `src/core/classPaths.ml`) are rebuilt with `readdir`
and `file_exists` calls; on top of that, the server `stat`s every cached module
file and every class path directory on each compilation
(`get_changed_directories`, `check_module` in `serverCache.ml`), and about 25
other places in the typer, display and server code access the file system
directly. The virtual file system (VFS) replaces all of this with a
server-wide, in-memory view of the file system, kept up to date by events.

1. **Content.** A tree of the directories the compiler cares about (class path
   directories, directories of module files, files declared with
   `registerModuleDependency`, wherever they are), each entry holding:
   - for a directory: its sorted listing (names and kinds);
   - for a file: size, full precision modification time, content hash, and
     the content itself while it is useful (see item 6);
   - an **overlay**: content sent by an editor for an unsaved buffer, which
     takes precedence over the disk content until the editor drops it.
     Overlays are part of the VFS from the start, so that display (deferred)
     can rely on them.
2. **Population.** A directory is listed (one `readdir` and its `stat`s) the
   first time the compiler needs it, and watched from then on. Server startup
   lists the class path roots; subdirectories are listed lazily.
3. **Updates.** The VFS is the only consumer of change sources:
   - the **file system watcher** (libuv `fs_event`: inotify, FSEvents,
     ReadDirectoryChangesW), on every directory present in the VFS;
   - **editor notifications** (`server/invalidate`, and file contents sent with
     display requests for overlays);
   - **writes made by the compiler itself** (generators, macros writing files,
     see item 7), applied to the VFS immediately (write-through) instead of
     waiting for the watcher's event, which is then recognized by its hash and
     ignored;
   - a **revalidation scan** (full precision modification time, then hash)
     only at startup and for directories the watcher cannot watch (network
     file systems, some container mounts, watch limits), reported once with a
     warning.
   Each update produces precise invalidations: `File(f)` for a content change,
   and the `DLookup` nodes whose resolution can change for a created, deleted
   or renamed entry (including a new file shadowing a type from a higher
   priority class path, or a negative lookup that now succeeds).
4. **Snapshot per compilation.** A compilation starts by draining pending
   events (coalescing bursts such as a `git checkout`), then works on a
   **frozen snapshot**: events arriving during the compilation are queued for
   the next one. Typing, including parallel typing (phase 3.5), therefore sees
   a consistent file system, and reads need no locking.
5. **Resolution without system calls.** The `resolve` query (phase 3) works
   on VFS listings only:
   - a persistent **resolution index** per context (platform extension, custom
     extension, core API loading, class path order) maps a relative path
     (`pack/Foo.hx`) to the most specific file, as `cache_directory` does
     today, but it survives across compilations and is updated entry by entry
     when a directory listing changes;
   - negative results ("`pack/Foo.hx` exists in no class path") are part of the
     index and of the `DLookup` dependencies;
   - case sensitivity follows the platform, with a deterministic error when a
     path only matches with a different case;
   - absolute paths outside the class paths (`Path.is_absolute_path` in
     `find_file_noraise`) are added to the VFS on first access and watched
     like the rest.
6. **Content retention.** The content of a module file is kept until it is
   parsed; afterwards only its hash is kept, since the parsed AST is cached
   (phase 3.6) and a changed file is read again anyway. Registered files
   (`DFile`) and overlays are kept while a node depends on them, within a
   memory budget measured in phase 5.
7. **All file access goes through the VFS:**
   - class path lookup (`classPath.ml`, `classPaths.ml`), module checks and
     shadowing (`serverCache.ml`), parsing (`TypeloadParse`), display and
     toplevel completion (`displayToplevel.ml`, `displayPath.ml`), native
     library and hxb loading;
   - the eval macro API: `sys.FileSystem.exists`, `stat`, `isDirectory`,
     `readDirectory`, `sys.io.File.getContent`/`getBytes` are served by the VFS
     for paths it knows (class path entries, registered files), from the
     compilation's snapshot; other paths go to the disk directly, uncached and
     untracked (reading files is the macro's responsibility unless it declares
     them with `registerModuleDependency`, phase 7, item 3). Macro writes
     (`saveContent`, `saveBytes`, `deleteFile`, `createDirectory`,
     `rename`) go through it as write-through operations, so that a file
     created or deleted by a macro is immediately visible to the next lookup
     without a system call (file writes themselves remain the macro's
     responsibility, phase 7, item 8);
   - generators: output files are written through the VFS, which keeps the
     hash of what was last written and skips writing identical content
     (phases 9 and 10) without reading the file back.
8. **CLI mode** (no server): the same API with lazy population and no
   watcher; it behaves like a per-process cache.
9. **Measurement and guarantee.** The VFS counts system calls per
   compilation (`--times`, `server/stats`). A compilation in which nothing
   changed, and the resolution of any path already known to the VFS, must make
   **zero** file system calls; `tests/incremental` checks this through the
   counter.

Exit criterion: equivalence across the whole corpus in history replay.
Measurement: number of retyped fields per commit, typing time.

---

### Phase 7: macros

With demand-driven typing (phase 3), macros run when their result is demanded,
not at a fixed pass. Macros relying on "all classes are built when I run" or on
a static registry being complete must move to the declarations of item 5 or to
`program()` stage callbacks (`onAfterTyping`, `onGenerate`). Build macro
cycles are errors (§ phase 3.3).

1. **Expansion key**:
   - expression macros: `(called macro field, hash of AST arguments, consumer field, call index in the body)`;
   - build macros: `(built type, macro, hash of input fields)`.
2. **Tracing** in `src/macro/macroApi.ml`: every API function that reads the
   program records a dependency for the current expansion (`getType` gives
   `DLookup` + `DHeader`, `typeof`/`typeExpr` the dependencies of the triggered
   typing, `getBuildFields` and `getLocalClass` the context, etc.).
3. **Interpreter sys primitives** (`src/macro/eval/evalStdLib.ml`):
   - file reads (`File.getContent`/`getBytes`, `FileSystem.exists`/`stat`/
     `readDirectory`) are **not tracked automatically**: a macro whose result
     depends on a file declares it with `Context.registerModuleDependency`,
     which records a `DFile` dependency of the expansion and adds the file to
     the virtual file system (watched, wherever it is). Reading a file without
     declaring it is the macro's responsibility, like writing files (item 8):
     a cached expansion is not re-run when that file changes;
   - non-deterministic calls (`Date.now`, `Math.random`, `Sys.getEnv`,
     `Sys.command`, `Sys.time`, sockets) are **not detected**: a macro whose
     result must be recomputed on every compilation calls
     `Context.markVolatile()`. Otherwise its cached result is reused, which is
     the macro's responsibility, like undeclared file reads;
   - console output (`Sys.println`, `trace`, `Sys.print`) is written
     immediately, in no guaranteed order when expansions run in parallel, and
     is **not replayed** when an expansion is reused from the cache;
   - file writes: not tracked (see item 8).
4. **Static state isolation.** Macro-context static variables no longer carry
   state from one macro call to another.
   - **Ordinary statics are reset for every expansion.** During an expansion, a
     static starts from its initial value; everything written is discarded when
     the expansion ends. Restoring the reference is not enough (a
     `static var cache = new Map()` is usually mutated in place), so the
     interpreter **re-runs the static's initializer on first access in each
     expansion**, lazily: statics an expansion does not touch cost nothing.
   - **Nested expansions** (a `typeExpr` or `getType` triggering another build
     or expression macro) get their own state, and the caller's state is
     restored on return (stack discipline).
   - **`@:persistent` means initialization value.** A `@:persistent` static can
     only be written during initialization macros (`--macro`). After
     initialization it is **frozen**, deeply: eval `Array`, `Map` and objects
     reachable from it get a frozen flag, and any mutation is an error. A deep
     hash of all `@:persistent` values after initialization acts like a define:
     if it changes, every expansion is invalidated. Functions (e.g. a
     `dynamic function` overridden by user configuration) are hashed by field
     reference, plus a `DInline` dependency on their body.
   - This changes the meaning of `@:persistent`, which today means "survives
     across server compilations". Existing uses as caches or registries become
     write errors after initialization, which forces their migration (item 9).
5. **Dedicated declarations** for state that must outlive an expansion (new
   module `haxe.macro.State`, names to be finalized):

   | Declaration | Write | Read | When it can be read |
   |---|---|---|---|
   | ordinary static | in the expansion | in the expansion | reset on every call |
   | `@:persistent` static | initialization only | anywhere | frozen after initialization |
   | `MacroArray<T>` | `push`, recorded per expansion | `gather()` | `program()` stages only |
   | `MacroMap<K,V>` | `set`, recorded per expansion | `gather()`, `get`, `keys` | `program()` stages only |
   | `MacroTypeData<T>` | the build macros of the type only | `get(type)`, which triggers the build | anytime |
   | `MacroCache<K,V>` | `getOrCompute(key, fn)` | `getOrCompute(key, fn)` | anytime (pure) |
   | `MacroIndex<K>` | `register(key)` | `indexOf(key)` returns a late-bound `Expr` | resolved after typing |

   - **`MacroArray<T>`**: each `push` is recorded under the key of the
     expansion that made it, and replayed when that expansion is reused from
     the cache. `gather()` returns the union in a deterministic order (sorted
     by expansion key, then push order). It may only be called once all
     contributors are known: in a `program()` stage callback or in a
     reduction. Calling it during typing is an error, since the result would
     depend on which expansions already ran.
   - **`MacroMap<K,V>`**: same as `MacroArray`, keyed. Two expansions writing
     the same key is an error unless a merge function is declared.
   - **`MacroTypeData<T>`**: a typed eval value attached to a type, written
     by that type's build macros. `data.get(T)` triggers the build of `T`, so
     it is deterministic and can be used **during typing**. It is the
     equivalent of metadata without encoding values as `Expr`. Writes are only
     allowed while the build of `T` runs (`members(T)`, a single query): a
     write from anywhere else, such as an expression macro in one of `T`'s
     fields, is an error. Since caching is per field, data contributed by
     several fields could not be kept consistent; that is also why there is
     no per-module equivalent. Expression macros of `T`'s own bodies can read
     what the build stored.
   - **`MacroCache<K,V>`**: pure memoization. `getOrCompute(key, fn)` runs `fn`
     as a sub-expansion recording its dependencies (registered files, types
     inspected…); the entry is invalidated when one of them changes. The
     engine may evict entries at any time, and reading an entry creates no
     dependency on the expansion that filled it.
   - **`MacroIndex<K>`**: global, stable indices needed inside generated code
     (index of a class in a registry, program-wide message ids…).
     `register(key)` records the key; `indexOf(key)` returns a placeholder
     `Expr` replaced by the integer constant once all keys are gathered (end of
     typing, before filters). Indices are assigned in sorted key order.
   - **`Context.makeUniqueName(base)`**: deterministic unique names, local to
     the expansion key, instead of static counters.
   - **Reductions**: a `program()` stage callback that calls `gather()` and,
     for example, `defineType`s a registry type runs on every compilation,
     with early cutoff on its result.
   - Whole-program APIs (`Context.getAllModuleTypes()`, iteration over loaded
     modules) have the same constraint as `gather()`: their result depends on
     what has been loaded so far, so calling them during typing is an error;
     they are allowed in `program()` stages. `Context.getModule(path)` stays
     allowed during typing: it loads one specific module (`DLookup` and
     `declare`).
6. **Recorded and replayed effects**: `defineType`, `defineModule`,
   `addMetadata`, `addResource`, `registerModuleDependency`, callbacks
   (`onGenerate`, `onAfterTyping`, `onTypeNotFound`), compiler messages
   (`Context.warning`, `Context.info`, errors; not console output), and the writes
   to the declarations of item 5 are logged per expansion. When the expansion
   is green, the log is replayed instead of running the macro.
   - **Metadata added to another type** (a type other than the one being
     built: `Compiler.addMetadata` from an expansion, `meta.add` on a type
     obtained through `getType`) is **late metadata**: it is logged as an
     effect of the expansion (replayed when the expansion is reused) and
     stored apart from the target's own metadata.
     - During typing it is **never visible**: macros and the typer only see
       declared metadata, or complete metadata after the target's build
       (§ phase 3.3). It therefore never changes `TypeHeader` or
       `TypeMembers`, and causes no typing invalidation.
     - After typing (`program()` stages, filters, DCE, generators) it is merged
       with the target's metadata, in a deterministic order (sorted by
       expansion key). Typical uses: `@:keep`, `@:native`, `@:expose` decided
       by a macro.
     - A new node `TypeLateMeta(t)` holds the merged late metadata with its
       fingerprint; post-processing and generation of `t` depend on it
       (phase 8).
   - **Modules loaded by an expansion** (`getType`, `getModule`, typing
     triggered by `typeExpr`…) are part of its recorded effects. Replaying a
     cached expansion demands the same modules again, so that the set of
     loaded modules, which `program()` types entirely, is identical in cold
     and warm compilations.
   - **Types defined by macros** (`Context.defineType`, `defineModule`) are
     owned by the expansion that defines them:
     - the type is an output of that expansion; it is replayed with it, and
       removed if the expansion is re-executed and no longer defines it;
     - a second expansion defining the same path is an error showing both
       owners. The common pattern `try getType(p) catch(_) defineType(...)`
       is only valid within a single owner; a helper type needed by several
       expansions (e.g. hxbit's `ObjProxy_…` classes) must be defined by a
       single owner, typically a `MacroCache.getOrCompute` keyed by the type
       path, whose cache entry is then the owner;
     - code referencing a macro-defined type by name records a dependency on
       the owning expansion (`DMacro owner`). A lookup that happens before
       the owner ran is order-dependent: it is reported by verification mode,
       and such references must go through the owner (or a `MacroCache`
       entry) to be deterministic.
   - **`Context.onTypeNotFound`** callbacks can only be registered during
     initialization macros (`--macro`), in a fixed order; registering one
     during an expansion is an error. Two callbacks answering for the same
     path is an error. The answer for a path is a query keyed by that path,
     so it is computed once and cached like any other node.
7. **Volatile macros**: an expansion that called `Context.markVolatile()` is
   re-executed on every compilation. If its output (and effect log) has the
   same fingerprint, downstream stays green.
8. **File writes are the macro's responsibility.** The compiler does not
   track, replay or verify files written by macros, and writing a file does
   not make an expansion volatile. A macro that writes files (heaps resource
   conversion in `hxd/fs/Convert.hx` and `hxd/fs/FileConverter.hx`, hide test
   generation, hide `ModelLibrary`) manages its own outputs and caches, as it
   does today. Files it reads are dependencies only when declared with
   `registerModuleDependency` (item 3).
9. **Corpus migration** (validates the declarations on real cases):
   - **domkit**: `COMPONENTS` (and `COMPONENTS_REMAP_REV`) become a
     `MacroTypeData<MetaComponent>` on the component class; `loadComponent`
     becomes `getType` then `data.get(t)`; `preload` and the
     `// force build` idiom disappear. `componentsSearchPath` and
     `defaultParserPath` stay `@:persistent` configuration, which requires
     `h2d.domkit.InitComponents.init` to run as an initialization macro
     (`--macro`) rather than as a `@:build` on `h2d.domkit.Object`.
   - **hxbit**: `SERIALIZABLES`/`UNSERIALIZABLES` are replaced by header
     information (interfaces, declared metadata, `@:native`);
     `markAsSerializable`/`markAsUnserializable` become initialization-time
     configuration. `IGNORED_META`, `VISIBILITY_VALUES`, `CUSTOM_GETTERS` and
     `wrapRPC` stay `@:persistent` configuration. The struct `isProxy`
     information becomes a `MacroTypeData`. The `PartialResolution` /
     `TLazy` workaround goes away.
   - **heaps**: `hxsl` stores the parsed shader in a `MacroTypeData` instead of
     re-parsing `:src` on every `@:import`; `hxd/res/DynamicText.hx`
     `BUILD_CACHE` and `hxd/fs/Convert.hx` `converts` become `MacroCache`s.
10. **Detection during the transition** (Haxe 5.x warnings, before isolation
    becomes the default):
    - cheap mode: eval tags each static with the expansion that last assigned
      it; reading a static assigned by another expansion emits
      `WMacroStaticState`;
    - `-D macro-state-check`: deep hash of everything reachable from macro
      statics before and after each expansion, which also catches in-place
      mutation (`map.set`); for CI and corpus audits;
    - history replay (phase 0) as the final safety net.
11. **Macro code**: the expansion depends on `DInline` (body) of every macro
    context function it executes. Simple first version: depend on the bodies of
    all macro-context modules reached during execution, as recorded by the
    interpreter.

Isolation makes expansions independent of each other, which is the
prerequisite for running them in parallel (§4b, item 3).

Exit criterion: Heaps, hxbit, domkit and hide pass history replay with static
isolation enabled, and at most a handful of identified and justified volatile
macros.

---

### Phase 8: incremental filters

1. **Post-processed copy**: filters work on a copy of `cf_expr`
   (`PostProcessed(cf)`) and no longer mutate the typed AST.
   `save_class_state`, `cl_restore` and `cf_expr_unoptimized` go away: the
   typed AST remains the source and is used for subsequent inlining.
2. **Local filters**, which only depend on the field and on interfaces: cached
   by `PostProcessed(cf)`, depending on `FieldBody(cf)`, `TypeLateMeta` of its
   type, the `DSig`/`DHeader`/`DMembers`
   read and the platform configuration. This covers most of `run_safe_filters`
   and `destruction_on_scom`, and the resolution of `haxe.PosInfos` nodes to
   line numbers (phase 2, item 6), which also depends on the field's anchor.
3. **Global passes, rewritten over per-field summaries**:
   - **Initialization order**: a graph of "static initializer or `__init__`
     of type A references type B" edges, built from summaries. Types are
     ordered topologically, ties broken by path. A cycle between static var
     initializers is an **error** showing the cycle; it must be broken
     explicitly (lazy initialization, `__init__`). This
     order is shared by all generators: types are output in it, all `__init__`
     blocks run first in this order, then all static var initializers in this
     order.
   - **DCE** (`src/optimization/dce.ml`): each post-processed field produces a
     summary "referenced fields and types, used features, `@:keep`".
     Reachability is a graph maintained incrementally: add or remove the edges
     of modified fields, then propagate the differences (mark or unmark).
     Acceptable first version: full reachability recomputation over summaries,
     very fast since no AST is walked.
   - **Descendants** (`add_descendant`): recomputed from headers.
   - **Features** (`has_feature`): union of the features of reachable fields.
   - **`RenameVars`**: the global part (reserved names and collisions) is
     computed over summaries. Per-function renaming is local, hence cached.
   - **Purity (analyzer)**: fixed point computed over the call graph of
     summaries, per strongly connected component. Only touched components are
     recomputed.
4. Phase output: for each reachable field, a `PostProcessed` node, and a global
   set (kept types, features, descendants) with its fingerprint.

---

### Phase 9: JS generation

`src/generators/genjs.ml` is split into:

1. **Per-type fragment**: the JS code of a class, an enum or an implemented
   abstract, plus its metadata (static initialization dependencies, `__init__`,
   used features). Cached by `Fragment(js, type)`, depending on the
   `PostProcessed` nodes of its fields, referenced native names and the global
   set (features).
2. **Per-fragment source map**, with generated positions relative to the
   fragment and source positions kept as anchored positions: they are
   resolved to file lines at link time, so a field that only moved in its
   source file does not need a new fragment.
3. **Linking**: header, type order and static initialization in dependency
   (topological) order with ties broken by path, all `__init__` blocks first,
   concatenation, source map offsets. Linear in output size, without walking
   any AST.
4. Only write the file if its content changed (to be shared with other
   targets).

Exit criterion: output byte-for-byte identical to cold mode (`tests/unit` on
JS, source maps included).

---

### Phase 10: HL generation

The heaviest part, because of the global tables in `genhl.ml` (`cglobals`,
`cstrings`, `cfunctions`, `cnatives`, `cached_types`, `anons_cache`,
`method_wrappers`…).

1. **Per-function compilation with symbolic references**: each function is
   compiled into a `fundecl` whose global operands are symbols
   (`SymString "foo"`, `SymGlobal path`, `SymFun (path, field)`,
   `SymType hl_type_key`) instead of indices.
2. **HL types**: a canonical, order-independent `hl_type_key` (today the type
   cache depends on encounter order). Class types and virtual objects
   (anonymous) are derived from `TypeMembers` and `FieldSig`.
3. **Per-function `hlopt`**: optimization cached with the fragment (the result
   only depends on the symbolic function).
4. **HL linker**: assigns indices (types, globals, strings, functions, natives)
   in a **deterministic order** (sorted by key), resolves symbols and writes
   the bytecode and debug information (anchored positions resolved to lines
   here, as for JS source maps). Linear in code size. The generated
   entry point runs all `__init__` blocks first, then static var initializers,
   both in the dependency order computed in phase 8.
5. **HL/C (`hl2c`)**: generates one C file per type from the linked fragments,
   and only writes modified files (avoiding C recompilation).
6. Risk: index order changes the binary output compared to the current mode.
   A new deterministic format is accepted, and equivalence is checked between
   incremental and cold **after** the phase.

Exit criterion: `tests/unit` on HL (JIT and HL/C) passes, with identical output
between incremental and cold.

---

### Phase 11: cleanup

- Remove module-level invalidation: `DependencyDirty`, `m_checked`,
  `m_cache_state` (or reduce it to a diagnostic summary), `m_processed`.
- hxb: used in verification mode. On-disk persistence (server restart, warm
  CLI compilation) and memory limits with eviction are decided after measuring
  the graph size (phase 5); if adopted, loading from hxb must restore
  fingerprints, anchors and the graph.
- User documentation: new errors (field types), macro state declarations, macro
  warnings, `incremental-verify` and `incremental-check-refs` defines.

## 4b. Parallel compilation

Parallelism is not limited to typing: every operation of a compilation runs on
the same scheduler. Today, parallelism exists in isolated places (filters and
analyzer per type, DCE marking, `genhl` function compilation, `hl2c` modules,
`genjvm` classes) and the pool is released while macros run
(`Parallel.ManagedPool.release` in `macroContext.ml`). The target is a single
model for the whole compilation.

1. **The query engine is the scheduler.** Every query is a task on a
   persistent work-stealing pool (Domainslib), kept by the server across
   compilations. Demanding a query that is not computed starts it; demanding a
   query running on another domain awaits it, and the awaiting domain runs
   other tasks in the meantime.
2. **Cycle detection across domains.** Each task carries its chain of
   demanding queries; awaiting a query that transitively awaits the current
   one is reported as the cycle error of phase 3.3, whatever the domains
   involved (wait-for graph).
3. **Operations and their unit of parallelism:**

   | Operation | Unit | Notes |
   |---|---|---|
   | File system events, scans, hashing (VFS, phase 6) | file / directory | events are drained before the compilation snapshot |
   | Parsing and `Decls` fingerprints | file | pure function of the content and of the defines read |
   | `declare`, `resolve`, `header`, `members` | module / type | build macros run inside `members` |
   | `sig`, `body`, `checks` | field / type | §3.5 |
   | Macro expansions | expansion | static isolation (phase 7, item 4) makes expansions independent; the eval interpreter keeps its compiled code shared and read-only, and its mutable state (static layers, call stacks, caches) per domain. Until this is done, macro expansions run on a single lane while other queries keep running |
   | Fingerprints | node | computed by the query that produces the node |
   | Post-processing (filters, analyzer) | field | already per type today; becomes per `PostProcessed(cf)` node |
   | Global passes (DCE, initialization order, features, descendants, `RenameVars`, purity) | summary / component | summaries are computed in parallel; reachability by parallel frontier; purity per strongly connected component |
   | Code generation | type (JS fragment), function (HL), C file (HL/C) | already parallel for HL functions and HL/C; other targets keep their current generators |
   | Linking | chunk | index assignment is sequential but linear; resolution of symbols and writing of chunks are parallel |
   | Output writing | file | through the VFS, skipping identical content |

4. **Rules for shared state:**
   - inputs of a query are immutable for its duration (VFS snapshot, results of
     the queries it demanded);
   - a query only writes its own result and the entities it owns (grafting is
     done by the owning `declare`/`members`/`sig`/`body` query);
   - global tables (module table, generic instances, resolution index, macro
     state declarations of phase 7, item 5) are concurrent maps with atomic
     insert-if-absent, so that two domains demanding the same key get the
     same result;
   - no global counter: identifiers are derived from stable keys (§3.4);
   - messages, lists and outputs built from several tasks are merged in stable
     key order, never in completion order.
5. **Determinism checks.** Output, messages and fingerprints must be identical
   with one domain, several domains, and a randomized scheduling order
   (`-D compilation-shuffle=<seed>`, which randomizes task order and
   work-stealing); CI runs `tests/incremental` and the unit tests in these
   modes.
6. **Configuration.** Enabled by default with one domain per available core.
   `-D compilation-domains=<n>` sets the number of domains, `1` runs
   everything on a single domain (same results).
7. **Measurement.** The scheduler reports per compilation the busy time per
   domain, the critical path (longest chain of dependent queries) and the time
   spent waiting, per operation, in `--times` and `server/stats`.

Each phase applies these rules to what it introduces: phase 3 for typing,
phase 6 for the VFS, phase 7 for macros, phase 8 for post-processing, phases 9
and 10 for generation.

## 5. Phase ordering and dependencies

```
Phase 0 ─┬──────────────────────────────────────────────────────┐
Phase 1 ─┤                                                      │
         ├─ Phase 2 ─ Phase 3 ─ Phase 4 ─ Phase 5 ─ Phase 6 ─┬─ Phase 8 ─┬─ Phase 9 (JS)
         │                                                    │           └─ Phase 10 (HL)
         │                                      Phase 7 ──────┘
         └─ (continuous verification on the whole corpus)                 Phase 11
```

The virtual file system (phase 6) only depends on phase 0 and can be built
early: it already fixes the change detection bugs of the current server
(edits within the same second, shadowing files) and removes its per-compilation
scans.
Phase 3 (demand-driven typing) is useful on its own: it removes order
dependence and enables parallel typing even without the server. Phase 2 comes
first because queries create and fill entities through the stable-key index.
Phase 7 (macros) starts after phase 5 (same recording mechanism) and must be
complete before enabling phase 6 on Heaps, hxbit, domkit and hide. Anchors are
introduced in phase 2, but matching moved declarations on reparse uses the
phase 4 `Decls` fingerprints and happens in phase 6.

## 6. Risks and open points

- **Size of the phase 3 refactor.** About 1,400 direct reads of entity
  contents to migrate behind abstract types. Mitigation: mechanical, module by
  module migration with a tracked escape hatch, and the cold-compilation
  equivalence check on the corpus after each step.
- **Macro code depending on pass order.** Macros that assume every class is
  built when they run, or that rely on cycles currently resolved by retries,
  will break with demand-driven typing. To be measured on the corpus early in
  phase 3 with the cycle instrumentation described in phase 3.3.
- **Parallel macro execution.** The eval interpreter must become safe to run
  on several domains (§4b, item 3); until then macro expansions run on a
  single lane, which limits speedups on macro-heavy code.
- **Parallel speedup.** Dependencies between queries bound the achievable
  parallelism (critical path); to be measured per phase with the scheduler
  statistics.
- **Dependency completeness.** The main risk: a missing dependency yields a
  silently wrong result. Mitigation: the "trace only" mode of phase 5 and
  history replay in CI.
- **Memory cost** of the graph: estimated at a few tens of bytes per edge. To
  be measured in phase 5 on Heaps; if needed, compact per module (integer
  arrays indexed by entity).
- **Recording cost** during cold typing: must stay low. Disabled outside the
  server.
- **Structural unification**: `DMembers` is coarse (adding a private field
  invalidates structural consumers). Possible refinement: separate public and
  private member sets.
- **Macros modifying other types** outside the logged effects (direct mutation
  through the `TypedExpr`/`ClassType` API of an object obtained via
  `getType`): detect it (write to an entity of another module during an
  expansion), and handle it with broad invalidation and a warning.
- **`@:multiType`, `@:genericBuild`, `onTypeNotFound`**: to be explicitly
  covered by tests.
- **Display** (deferred): check that completion is not slowed down by
  validation in the meantime.
- **Static initialization order** changes compared with the current compiler
  (dependency order, `__init__` first): a behavior change to document and to
  test on the corpus.
- **Metadata added to other types** is invisible during typing: macros that
  add metadata to another type and expect a later macro to read it during
  typing will silently stop working. To be detected on the corpus (a read of
  metadata that only exists as late metadata of that type can be reported in
  `-D macro-meta-check` mode).
- **Inference cycles** between fields without annotations remain frequent
  even with the syntactic `Void` rule (39 distinct cycles in the corpus, phase
  3.3 measurements): one annotation each, to be fixed in the standard library
  and announced to library authors with the Haxe 5.x warning.
- **Static initialization cycles** become errors: existing code with circular
  static initializers must be fixed.
- **Deterministic HL format**: a visible change for tools reading bytecode
  (debugger, profiler), to be validated with them.

## 7. Tests to add

- `tests/server`: one case per dependency kind (signature change, body change,
  inline body change, shape change, file shadowing a type, changed define,
  file read by a macro…), checking recomputed nodes through server statistics.
- `tests/misc`: phase 1 errors (non-inferred field types) and phase 3 cycle
  errors (headers, build macros, recursive inferred types).
- Order-independence tests for phase 3: compile with queries demanded in
  shuffled order (`-D typer-shuffle=<seed>`) and in parallel, and compare.
- `tests/incremental` (in place): scenario cases, to be extended with every
  phase; `tests/incremental/replay`: history replay harness (phase 0) on
  `tests/unit`, Heaps, hxbit, domkit and hide.
- Position tests: insert lines above unchanged fields and check source maps,
  HL debug info, `haxe.PosInfos` line numbers and cached error positions.
- Macro state tests: static isolation (including in-place mutation and nested
  expansions), `@:persistent` freezing, each `haxe.macro.State` declaration,
  metadata reads with and without `Context.buildType`.
- Static initialization order tests (dependency order, `__init__` first,
  cycle errors).
- Macro-defined type tests: replay with the owner cached, owner re-executed
  without the definition (type removed), second definition of the same path
  (error), `onTypeNotFound` registered outside initialization (error).
- File change detection and virtual file system tests: edits within the same
  second, new files shadowing a loaded type, deletions and renames, directories
  created and deleted, bursts of changes, files created and deleted by macros
  and read in the same compilation, case-only renames, unwatchable directories
  (scan fallback), and zero system calls for a compilation without changes
  (system call counter); the `tests/incremental` cases marked
  `@real-mtime` and the shadowing cases must pass without the runner's
  modification time adjustment.
- Parallel compilation tests: identical output with `-D compilation-domains=1`,
  with several domains, and with a randomized scheduling order
  (`-D compilation-shuffle=<seed>`), on the whole `tests/incremental` suite;
  cross-domain cycle detection.
- Determinism tests: stored typed expression ids, virtual file names,
  `Compiler.include` order, error message texts and anonymous structure names
  identical between cold, warm and shuffled-order compilations.
- Macro API restriction tests: `getAllModuleTypes` during typing (error) and in
  `program()` stages, metadata added to other types (invisible during typing,
  merged in `program()` stages, deterministic order).
- Parse sharing tests between the macro and target contexts (`#if macro`,
  defines with different values).
- Unit tests for the hasher and grafting (old ↔ new mapping, remapping, stale
  pointer checker).
