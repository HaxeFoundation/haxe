# Incremental compilation: implementation plan

Goal: the compilation server only retypes, post-processes and regenerates what
actually changed, with a result **bit-for-bit identical** to a cold compilation.

## 0. Decisions

| Topic | Decision |
|---|---|
| Inference unit | **The field.** A field's type must be fully determined at the end of its own typing. A monomorph still open in a field's type (`static var x = []`) is an **error**: a type annotation is required. A function's signature is known without typing its body when every argument is annotated or **typed by a literal default value**, and its return type is annotated or a syntactic `Void` (§3.3). |
| Granularity | **The field**, directly: dependencies, fingerprints and retyping are per field. |
| Scope | Typing, filters and generation. |
| Targets | **JS and HL only.** Other targets (C++, JVM, Python, Lua, PHP, Neko, Flash) are **out of the scope of this plan**: they must keep compiling with a global generation step, but nothing in this plan makes their generation incremental. |
| Macros | Automatic tracing of compiler API reads; **volatility is explicit** (`Context.markVolatile()`), like file dependencies; volatile expansions are re-executed with early cutoff on their output. Console output (`Sys.println`, `trace`) is printed immediately, in no guaranteed order, and not replayed from the cache. **Macro statics are reset for every expansion**; `@:persistent` is reserved for initialization values (frozen afterwards); state that must outlive an expansion uses dedicated declarations (`MacroArray`, `MacroMap`, `MacroTypeData`, `MacroCache`, `MacroIndex`). File dependencies are **explicit**: only files declared with `registerModuleDependency` are tracked (and watched, wherever they are); reading or writing other files is the macro's responsibility. |
| Target release | **Haxe 6.** Work happens on the long-lived `haxe6-incremental` branch, regularly merging `development` in. New warnings (phase 1, cycles) may be backported to a Haxe 5.x release as a transition. |
| Compatibility | Breaking changes accepted for Haxe 6. The new rules are **warnings in Haxe 5.x** and **errors in Haxe 6, with no compatibility flag**. |
| Macro context and configurations | Every context on a server (the macro context, the target context, and other configurations such as js and hl, or debug and release) keeps a **separate query graph** (different defines, platform and standard library overrides), but they all **share parsing**: a parsed module is reused by another context when the defines read during its parsing have the same values in both (phase 3.6). |
| Static initialization order | Types are output and static vars initialized in **dependency (topological) order**, ties broken by path. All `__init__` blocks run in a **first pass**, before any static var initializer, in the same order. A dependency cycle between static initializers is an **error**. |
| Error reporting | **Configurable.** By default every independent error is reported: a failed query is marked failed, its dependents are skipped without reporting cascading errors, and typing continues. A flag stops after the first (or N) errors, in deterministic order. |
| Storage | **In memory only** at first, preserving the physical identity of objects, with no memory limit and no on-disk persistence: the graph size is measured before deciding on eviction or persistence. hxb is not on the critical path. The compilation server **always** keeps modules in memory: the former hxb cache, which serialized modules into hxb chunks kept in the server's memory, is removed (`-D hxb-cache`, `-D disable-hxb-cache`, `-D disable-hxb-optimizations`). hxb only has a role as an **on-disk backup** of that state, independent of the in-memory storage (§6, persistence). |
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
exist (phase 4). Item 2 is in place: `-D incremental-check-refs`
(`src/compiler/refCheck.ml`) reports every type, field or enum constructor
reference that does not point to the object its module currently holds. Note
that the server's default cache decodes cached modules from hxb on every
compilation (new objects each time), so the check is meaningful with the
in-memory cache, which is the storage chosen by the plan and, on this branch,
the server's only cache (see the storage decision). `tests/incremental` can run in that mode with
`--args "-D incremental-check-refs"`: no stale reference
is reported. The one pre-existing warm/cold difference of that mode
(`macros/add-resource` on js: DCE dropped `js.Boot` and every feature-dependent
`__name__` on a module reused from memory) is fixed by the first step of
phase 2, item 3 (see "DCE markers" there).

**First replay** (last 30 commits of Heaps, compiled to JS with `include` of
`h3d`, `h2d`, `hxsl`, `hxd`, with hxbit, domkit, castle and format):
- 29 of 30 commits produce a JS file **identical in content but not in type
  order** between the warm server and a cold compilation; the same happens
  with the current nightly compiler, so this is pre-existing: the output order
  of the JS generator depends on the order in which modules are typed or
  restored from the cache. This is the deterministic output order required by
  the plan (§0, static initialization and output order; §3.4);
- with `--ignore-order`, all 30 commits are identical;
- **cause and fix**: `Finalization.sort_types` orders types by walking static
  initializers, before the filters. A module reused from the server's cache
  holds post-processed expressions (principle 4 is not met yet), e.g.
  `new haxe.ds.StringMap()` instead of the abstract `Map` constructor, so the
  walk found dependency edges that a cold compilation does not see. The order
  is now computed again after the filters (`Finalization.sort_type_list`,
  called by `Filters.destruction`), when every type has post-processed
  expressions, as the plan prescribes for the initialization order (phase 8).
  The replay is then identical in strict mode (30 of 30 commits), and
  `tests/incremental/cases/ordering/` reproduces the bug (both cases fail with
  the previous compiler);
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
- `var x = dynamicExpr;` (an initializer of type `Dynamic`, e.g.
  `neko.Lib.load(...)`): the field is `Dynamic`. Today its monomorph stays
  open and is bound by its first use (often to a function type); in Haxe 6 it
  is bound to `Dynamic` at the end of the field's typing. This removes 168 of
  the 175 cases of the Neko standard library.
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

**Status: errors (phase 1b done).** A field whose type is not fully known at
the end of its own typing, and a field reading the type of another field
while that one is being inferred (inference cycle), are **errors**; the
`WUnresolvedFieldType` warning is removed. Build order problems (build
retries, partially built classes read by macros) stay `WTypingOrder`
warnings until demand-driven typing (phase 3) makes them cycle errors, since
`h2d.domkit` relies on them. No cascading error: a field whose typing
reported an error is not also reported as unresolved (`Common.reported_errors`).
- The repository's tests, the CI runner and the standard library are
  annotated (unit tests on every target, misc, server, optimization, sys);
  tests whose subject is an unresolved type expect the new error.
- The server test builder types the argument of test methods (`_`) as
  `utest.Async`.
- Third-party code compiled by the CI is annotated: hxcoro on its
  `haxe6-annotations` branch (`Config.hxcoroVersion`); utest, tink_core,
  tink_macro, tink_unittest and deep_equal through patches in
  `tests/runci/patches`, applied by `System.patchLibrary` after installation
  until upstream has them.
- The most frequent case in user code is a build macro without return type
  that returns `null` (`function build() { ...; return null; }`): its type
  is `Null<Unknown>`, an error that asks for `:Array<Field>`.
- Two regressions of the deterministic output order (phase 0) were found on
  the way and fixed: `Finalization.sort_type_list` identified types by path,
  so a type renamed by `@:native` to the path of another type was dropped
  (`tests/misc/jvm/projects/Issue10280`, `tests/misc/neko/projects/Issue4679`).

**History.** `WUnresolvedFieldType` was first implemented
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

**Corpus libraries.** Annotation patches are on a `haxe6-annotations` branch of
each library: Heaps, hxbit, domkit, castle and hscript (they also compile with
the current Haxe nightly). With them, a JS build of Heaps including `h3d`,
`h2d`, `hxsl`, `hxd`, `hxbit`, `domkit` and `cdb` reports no
`WUnresolvedFieldType` nor `WTypingOrder` warning. (`cdb.SqlParser` does not
compile outside Neko, independently of this plan: `#if neko c != null #end &&`.)

**hide** (`hide.hxml`, first target, with the patched libraries): 100 warnings.
- 75 `WUnresolvedFieldType` in hide itself (`hide`, `hrt`), mostly fields
  initialized to `null` (44) or `[]` (14), plus one in `format` and one in the
  `hashlink` library: annotations, to be done in hide when it moves to Haxe 6.
- 9 inference cycles in hide.
- **15 build order warnings in `h2d.domkit.BaseComponents`**: the domkit
  component build macro of each component reads its parent component class
  while it is being built (`A macro reads class h2d.domkit.DrawableComp while
  it is being built`), and builds are retried (`Build of
  h2d.domkit.BitmapComp is retried later because it depends on
  h2d.domkit.DrawableComp, which is being built`). With the Haxe 6 rule
  (every query cycle is an error, including build macro cycles), this is the
  main breaking point of the corpus: `domkit.Macros` must demand `members` of
  the parent component (a regular dependency, completed first) instead of
  reading its partially built class, which phase 3 makes possible. To be
  designed with domkit's `preload` mechanism.

**Rules applied to typing.** Three rules that make a field's signature known
from its declaration are applied by the typer of this branch (not only
measured): a function without `return <expr>` and without return type hint
has a `Void` return type (syntactic `Void`), an argument without hint whose
default value is a literal determining its whole type (`""`, `0`, `1.5`,
`false`, `-1`) has that literal's type, and a variable initialized with a
`Dynamic` expression is `Dynamic` (`setup_args_ret` and
`bind_var_expression` in `src/typing/typeloadFields.ml`, test in
`tests/misc/eval/projects/haxe6FieldRules`). The unit tests (js, macro, hl),
the server and misc tests, the incremental suite and Heaps compile
unchanged; the only visible difference is a more precise error message
(`() -> Void` instead of `() -> Unknown<0>`, `Issue6796`). Macro functions are
not affected (their signature is rewritten to `Expr` or `Dynamic`).

**Standard library: done.** All reported fields of the standard library are
annotated, for every target compiled by `tests/unit` (macro, js, hl, hlc, cpp,
php, lua, jvm, neko): 7 common fields (`Serializer.BASE64_CODES`,
`Unserializer.CODES`, `InflateImpl.FIXED_HUFFMAN`,
`Template.hxKeepArrayIterator`, `SuspensionResult.withError`, eval
`ThreadImpl.getName`, `js.Boot.__resolveNativeClass`), 9 target-specific ones
(HL `Tls`, `Deque`, `Mutex` natives, Lua `Boot.clampInt32` and
`Socket.timeout`, PHP `Boot.__get`, JVM `Enum._hx_getParameters`) and 7 Neko
ones, plus the return types breaking the inference cycles of
`haxe.macro.Printer`, `haxe.Template`, `haxe.rtti.CType` and
`haxe.zip.Huffman`. `-w +WUnresolvedFieldType -w +WTypingOrder` reports
nothing in `std` on these targets.


---

### Phase 2: stable identity and content separation

Goal: be able to mutate an entity in place without breaking other modules'
pointers.

1. **Entity keys** (§3.1) and a per-module `entity_key -> object` index in
   `CompilationCache`.

   **Done.** `src/core/entityKey.ml` defines the key
   `(module, type path, kind, name, overload index, type parameter index)`,
   with kinds `type`, `static`, `member`, `constructor`, `enum field`,
   `structure field` (fields of the structure a typedef declares) and
   `type parameter` (of a type, a field, an overload or an enum constructor).
   Abstract implementation classes (`_Impl_`) and module-level fields
   (`_Fields_`) are types of their module, keyed by their own path.
   `EntityKey.iter_module` enumerates a module's entities with their keys and
   `context_cache#get_entity_index` keeps the index of the current version of
   each module. The index describes the module *as typed*: it must be built
   while the module's classes hold their typed fields, not their DCE-filtered
   state (grafting must restore an old module before matching it).
   `-D incremental-check-refs` checks that keys are unique in every module
   (no collision on the unit tests for JS, HL and macro, nor on Heaps with
   hxbit and domkit) and reports each stale reference with its key, and
   whether the current module has an entity with the same key.
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

     **Status.** Already the case: the filters rebuild `cl_descendants` from
     every type of the compilation (`Filters.run`), and restoring a cached
     class (`cl_restore`) empties it; a grafted class gets the empty list of
     its new version. The only read during typing (`Calls`, forced inline
     calls) is delayed to the filters.
   - DCE and post-processing flags (`CfUsed`, `CfPostProcessed`, …): moved to
     post-processing data (phase 8), out of `cf_flags`.

     **DCE markers (done, first step).** Every marker that DCE sets is now
     reset by DCE itself when it collects its entry points, instead of relying
     on the restore functions of cached types: `:used` and `:directlyUsed` on
     enums and abstracts, `:valueUsed` on abstracts, and `CfMaybeUsed`. Enums
     reused from memory kept the `:used` of the previous compilation, so
     `mark_enum` did nothing and the `has_enum` feature (which keeps
     `js.Boot.__string_rec`) was lost. Typing-time and DCE-time markers are
     now distinct:
     - `EnUsed` (enum flag) replaces the `:used` metadata on enums;
     - `CfMaybeUsed` is DCE-only (a field that is kept if its class is kept);
     - `CfStructurallyUsed` is the typer's marker: structural unification of a
       class instance or `Class<T>` with an anonymous structure (it used to set
       `CfMaybeUsed`, so DCE could not reset it).

     Other DCE state on cached entities, fixed the same way:
     - `m_features` (the features a module's code uses, recorded by DCE
       marking) is reset by DCE for every module it processes; a cached module
       used to keep the features of earlier compilations
       (`cases/dce/cached-module-features.hxtest`: `$hxEnums` kept in the JS
       output after the last use of an enum was removed);
     - `CfOverride`, which DCE removes from a field whose parent field it
       removed, is now part of the state saved by `save_class_state` and
       restored when the class is reused; the HL generator reads it to build
       virtual tables (`cases/dce/cached-override-flag.hxtest`, HL bytecode
       differed).

     With these fixes, the whole `tests/incremental` suite passes on JS,
     interp and HL (`--add-target hl`, bytecode compared), with both the hxb
     and the in-memory cache.

     Remaining: `CfStructurallyUsed` is stored on the *target* field by the
     typing of another module. When that module changes and no longer unifies
     the class with a structure, the marker stays, and a warm compilation can
     keep more fields than a cold one. It becomes a fact of the using field's
     `body` result, aggregated in phase 8. Abstracts keep their markers as
     metadata (no flag field) until phase 8.
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

     **Status.** Moved to phase 3: an instance is a query keyed by the generic
     entity and its type arguments (§3.5), whose result is stored outside the
     host class and added to it by post-processing (phase 8). Until then, a
     generic method instance is a field added to the host class by the typing
     of the calling module; it is dropped when the host's module is retyped,
     which is sound because the calling module depends on the host's module
     and is retyped as well.
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

   **Status of items 4 and 5.** Grafting is active with the in-memory cache
   (the default cache); it is unconditional (no define).
   `src/typing/graft.ml` grafts *at creation*, which removes the remapping
   step: when a module whose previous version is in the in-memory cache is
   retyped, each new type (class, enum, typedef, abstract), class field
   (members, statics, constructors, overloads by declaration order) and enum
   constructor is copied into the object of the previous version with the same
   entity key as soon as it is created, before anything refers to it, and the
   old object is used from then on. The previous version is first restored
   (`mt_restore`) so that fields removed by DCE are matched too. The only
   self-reference created before grafting, the `Class<T>` type of a class's
   statics (`cl_type`), is rebuilt on the old object. The eval interpreter
   recognized a retyped type by a new object; it now compares the module id it
   recorded when it added the type.
   - Type parameters are grafted with their `KTypeParameter` class (both refer
     to each other, so both references are rebuilt), for types, methods and
     constructors (from the key the field got when it was grafted) and enum
     constructors; so are the fields of the structure a typedef declares
     (`cases/types/graft-params-structures.hxtest`).
   - Not grafted yet: type parameters of structure fields, local functions
     and higher-kinded parameters, structures nested in a typedef's structure,
     fields created outside field initialization (property accessors
     generated for interfaces, `@:overload` metadata, `@:generic` instances,
     fields added by filters).
   - `-D incremental-check-refs` also checks type parameter classes (module
     of their host) and the fields of a typedef's structure read through a
     value of that (unparameterized) typedef.
   - `-D incremental-retype-changed-only` (development mode, unsound) makes
     the server retype only the modules whose own file changed, not their
     dependents: it shows which references grafting keeps valid without
     retyping the modules that hold them.
   - Results: `tests/incremental` passes entirely with
     `-D incremental-check-refs`
     (no stale reference). With `-D incremental-retype-changed-only` in
     addition, the only stale references left are shape changes (a removed,
     renamed or moved field, a removed `@:from` cast, a class that no longer
     implements an `@:autoBuild` interface), which phase 5 handles by retyping
     the dependents, and `@:generic` method instances (item 3). A replay of
     the last 13 commits of Heaps with the in-memory cache, grafting and the
     checker gives identical output for every commit and no stale reference.
   - The server statistics (`-v`) print the number of grafted entities.
   - Next: relative positions (item 6), and `@:generic` instances (item 3).
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

   **Status: `pos` is abstract** (`Globals.Pos`, one commit over the whole
   compiler). Its representation holds the file, the offsets relative to an
   anchor and the anchor (`{ mutable delta : int }`); `Pos.file`, `Pos.min`
   and `Pos.max` return absolute values, `Pos.make` builds a position (with
   the shared `Pos.no_anchor` by default) and `Pos.with_file`,
   `Pos.with_min`, `Pos.with_max` and `Pos.with_range` derive a position
   from another one, keeping its anchor and taking absolute offsets. Every
   position still has `Pos.no_anchor`, so the compiler behaves exactly as
   before (unit tests for js, macro and hl, server tests in both cache modes,
   misc, sourcemaps, nullsafety, optimization and incremental suites).
   Structural equality and hashing of positions compare relative offsets and
   anchors: identical to absolute comparisons while no anchor is shifted.

   **Status: anchors allocated per declaration.** After parsing a file,
   `TypeloadParse.anchor_declarations` gives one anchor to each field and
   enum constructor, one to each type declaration header and module-level
   field, and one shared anchor to the module's imports and usings
   (`Pos.anchor_all`, a walk over the declaration's parsed AST setting the
   anchor of the positions of that file that have none yet). Anchors are
   created with a delta of 0, so absolute offsets are unchanged. Cost on
   Heaps (h3d, 1295 files): parsing goes from about 0.59s to 0.68s (about 2%
   of the compilation); allocating the anchors in the parser itself would
   remove the walk. Positions of the typer derived with `Pos.with_*` keep the
   anchor of the position they derive from.
   Next steps: hxb writing offsets relative to the anchor, `haxe.PosInfos`
   resolved after typing (phase 8), comparisons that must stay absolute once
   anchors move (`Pos.compare`, display, statistics), and shifting anchors on
   reparse (phase 6).

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

**Status: query engine (first step).** `src/context/query.ml` defines
queries (`header(T)`, `members(T)`, `sig(f)`, `body(f)`), their state
(`NotStarted | Running | Done | Failed`) and the query stack. Demanding a query
that is running raises a cycle error showing the chain of queries, for
example `Cycle detected while building types: members(MixedA) ->
sig(MixedB.f) -> members(MixedA) (field x is read while MixedA is being built)`
(`tests/misc/eval/projects/buildCycle/compile3-fail.hxml`: a build macro of
`MixedA` types `MixedB.f()`, whose body reads a field of `MixedA`; this used to
report `Class<MixedA> has no field x`). Running as queries: the header and the
build of classes (`TypeloadModule`), and the typing of fields (`sig` when the
type is inferred from the body, `body` when it is declared). The typing was
then still scheduled by the passes; the `program()` driver below replaces
them.

**Status: order-independence check.** `-D typer-shuffle=<seed>` runs the
delayed tasks of each pass in a random order (seeded), so that every
remaining dependence on task order shows up before the passes are replaced by
demanded queries. On `tests/unit` (macro), it found and fixed:
- inherited `@:using`: copied from the parents by a task that had to run
  after theirs; now computed when read (`TFunctions.type_usings`);
- `@:using` metadata and module `using` statements: one task per entry,
  each prepending to the list, so the precedence (the latest one wins)
  depended on task order; each type or module now resolves its pending ones
  together, in declaration order;
- default constructors inherited through several classes (`Issue4557`): a
  class now adds its super class's pending constructor first;
- pending field bodies (see above): the declared type is used directly.
With twelve seeds, the only remaining failure is `Issue3914`, two fields
whose macros communicate through a macro static, which is phase 7 (macro
statics isolation).
On the corpus (Heaps with hxsl, hxbit and domkit macros, and hide), it also
found:
- inference cycle errors in the macro context depending on the order
  (`hxsl.Eval.handleReturn`, `domkit.CssParser`): fields of the macro
  context still typed their body when their declared type was read; they now
  follow the same `sig`/`body` separation (the macro context is typed before
  any macro runs, so bodies are available when needed);
- the order of the accessors generated for interface properties, now placed
  right after their property (getter, then setter);
- the order of `@:generic` method instances, now placed after their generic
  method, sorted by name.
The generated JS of Heaps and hide is now identical with eight seeds and
without shuffling. (A tempting fix, flushing only the structure of loaded
modules while a field is inferred, broke default constructors inherited from
a private constructor in hide: the connection tasks are needed as soon as a
loaded type is used.)

**Status: `program()`, first step.** The bodies of a class are no longer
one field typing task each: when its build ends, a class adds a program unit
(the typing of its field bodies, in declaration order) to a queue, and
`Typecore.program`, a single task of the field typing pass, types the units
in the order the classes were built until none is left (classes loaded while
typing add theirs). The other delayed tasks are not migrated yet. This
change of schedule revealed three more order dependences, now fixed:
- the access check of constructors treated a constructor whose body was not
  typed yet as an inherited one (`Issue5871`); inherited constructors are
  now marked (`CfInheritedConstructor`);
- macros reading `ClassField.type` got an unforced lazy type for fields not
  typed yet, and `null` instead of the type (hide's prefab properties):
  reading `type` now demands the field's signature (a type being inferred
  keeps its lazy encoding);
- compiler messages were not sorted after all: the CLI and server output
  paths did not use the sorting function; they do now (the expected output
  of 30 `tests/misc` cases changed order only).
A stored typed expression id printed by `Issue11004` changed: these ids are
still a global counter (phase 3.4, with phase 7).
The per-type checks run after the bodies (`checks(T)` in §3.1: property
accessors, core API, overloads, abstract class implementation, interface
implementation, final variables, recursive abstract underlying types) are
`program` checks, run in the order they were added
(`Typecore.add_program_check`). A recursive abstract underlying type is now
reported on the first abstract of the cycle by position, whichever check
finds it.

**Status: `program()` driver.** The typer passes are no longer lists of
delayed tasks run last-in first-out: they are the stages of `program()`
(`Typecore.program_stage`), one per pass, each a queue run in the order its
units were added (then its late units, in the order they were added). The
stages are, in order: the structure of modules and types (module build, class
builds, connection of fields: accessors, interface checks on classes built
while their build macro was running, `@:using`, import resolution, inherited
documentation, pending default constructors), the bodies of fields (one unit
per class, in the order classes were built, plus the checks of extern
function arguments, the late checks of `haxe.extern.Rest` arguments and of
variables whose type may be inferred as `Void`, the bodies of methods read
from `.hxb` files and the JVM strict metadata), the type parameter constraint
checks, then the per-type checks and the forcing of the remaining lazy types.
A unit can add units to any stage; the earliest stage with units left always
runs first (`run_stages`), so loading a module while typing a field runs the
structure of its types first: that is the demand of their `members` queries
until accessors demand them one by one (§3.2). `-D typer-shuffle` now picks
the units of a stage at random.

Running the units in the order they were added instead of the reverse order
changed which of two entities is processed first where the outcome depends on
it: a recursive constraint between two type parameters is now reported on the
constraint of the first one, and two classes with the same `@:autoBuild`
macro are built in declaration order (`Issue11870`, `Issue9381`). (A build
cycle between two classes, `Issue4671`, was then reported on the second
declaration; since the accessors of §3.2 demand headers, it is reported on the
first one again.) In hide, domkit numbers CSS properties with a
macro static counter in the order components are built, so the generated
handler registrations changed order (declaration order now); this is a
macro static shared between expansions (phase 7). The output is identical
with any `typer-shuffle` seed.

**Status: `members(T)` of classes.** The connection of the members of a class
(accessor checks, which also generate the accessors of interface properties,
the statics of the class type, inherited documentation and the default
constructor inherited from the super class) is no longer a set of connection
tasks run whenever that stage is flushed: it runs when the build of the class
ends, as the end of its `members` query, so whoever gets a class built gets
its members complete. Classes read from `.hxb` files follow the same rule:
building one reads the fields of its module, which was a connection task
before (a class inheriting its constructor from a class of an `--hxb-lib`
got an incomplete constructor once the inheritance ran at build end,
`tests/misc/hl/projects/Issue12357`). What stays in the connection stage is
what is not attached to the build of a class: the module-level import and
`@:using` resolution, the documentation of enums and abstracts, the forcing of
type parameter constraints, the interface checks of classes whose build
macro is running, and display requests.

This completes §3.1: every piece of typing is a query (`header`, `members`,
`sig`, `body`) or a unit of a `program()` stage run in a deterministic order.
Loading a module while typing a field no longer runs the whole structure
stage: since §3.2, reading the members of a type demands them.

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

**Status: accessors that demand the queries.** `src/core/tAccess.ml` groups
the accessors by query: `Header.super`, `Header.implements` (demand the
header of a class), `Members.fields`, `statics`, `ordered_fields`,
`ordered_statics`, `constructor`, `find_field`, `find_static` (demand the
build of a class, its `members` query), `Members.enum_constructors`,
`enum_names` and `Members.abstract` (the members of an abstract: its
operators, array access, casts and constructor, from its implementation class
and, for a module read from a `.hxb` file, from the rest of the module, which
`m_extra.m_complete` reads). Every read of these contents by the typing code
that consumes types goes through them (`src/typing`, `src/context`,
`src/coro`, and `TFunctions.raw_class_field`, `TUnification`), about 230
reads migrated by a script and the few patterns by hand. The query
implementations (`TypeloadFields`, `TypeloadModule`, `TypeloadFunction`,
`Generic`, `Graft`) read the type they compute directly, and so does the code
running after typing (filters, generators, statistics): that is the frozen
view of item 4, for now by convention.
With the reads demanding the queries, loading a module while typing a field no
longer runs the whole structure stage (`complete_structure`, removed from
`load_module`): the types are built when they are read. The structure stage
still runs before a function body is typed (`TypeloadFunction`, the
initialization of variables, macro calls), for the connection units not
attached to a type (import and `@:using` resolution, enum and abstract
documentation, constraints): without it, `hxcoro` and Heaps' domkit macros
fail. A class whose module
is being created (before its build is set up) reads as empty instead of
failing (`TAccess.not_ready`), as display requests do. A build cycle between
two classes is reported on the first declaration again (`Issue4671`). The
`@:using` metadata of a type is resolved when its usings are first read
(`TAccess.force_usings`, from `TFunctions.type_usings`), no longer only by the
structure stage: a static extension of a type loaded while typing a field was
not found otherwise (`eval.luv.Result.resolve` in hxcoro).
Not done: making the entity records abstract (item 1), which would turn the
convention into a compile-time rule, and the recording of dependencies by the
accessors (item 2), which is phase 5's job; the accessors are the places
where it goes.

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
  signature. Likewise, an argument without hint whose **default value is a
  literal** that determines its whole type (`tabs = ""`, `n = 0`, `x = 1.5`,
  `b = false`, `n = -1`) is typed by that literal, without typing the body.
  A default value that only determines part of the type (`v = null` gives
  `Null<?>`, `a = []` gives `Array<?>`), like an optional argument without
  hint (`?parent`), fixes that part, and the rest is inferred from the body:
  the signature then depends on the body. The only exception is a field referring to itself
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
build retries (now errors, see below), a macro reading a class
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

With the literal default value rule (decided, §3.3) and the instrumentation
counting a partially known type (`Null<?>`) as inferred, Heaps `master`
(js, `h3d`, `h2d`, `hxsl`, `hxd`) goes from 10 to 8 cycles: the two lifted ones
only depended on `tabs = ""`-like arguments, and the remaining ones go through
a return value (`hxsl.Printer.toString`, `hxsl.Checker.makeVar`,
`hxd.fmt.fbx.Parser.parseNodes`) or an optional argument without hint
(`hxsl.Printer.addVar(…, ?parent)`). With the standard library annotations of
phase 1 and the Heaps patch (`haxe6-annotations` branch of Heaps), Heaps reports
none.

**Status: build cycles are errors.** `Build_canceled` and the rebuild loop are
removed. A class whose super class or interface is still being built (its
build, or one of its build macros, is running higher on the stack) is a cycle
error showing the classes being built, for example
`Cycle detected while building types: Base -> Child -> Base (Child needs Base,
which is being built)` (`tests/misc/eval/projects/buildCycle`). The only case
in the corpus was `h2d.domkit` with hide: domkit's `buildObject` forced the
build of every preloaded base component from the first component built, which
could be one of them; the `haxe6-annotations` branch of domkit only preloads
from a component that is not in the list. A macro reading another class
that is being built (a class on the build stack other than the class whose
build macro runs) is a cycle error as well, for example `Cycle detected while
building types: A -> B -> A (a macro reads A, which is being built)`
(`tests/misc/eval/projects/buildCycle`, `compile2-fail.hxml`); none in the
corpus. Reading the fields of the class being built through `getLocalClass()`
is still allowed: it becomes an error with the lazy macro `ClassType`.

**Status: class headers and the lazy macro `ClassType`.** A class's header
(super class, interfaces, declared metadata) is computed separately from its
build (`cl_build_header`, `TypeloadModule`): the build computes the header
first, and macros compute it alone. In the macro API, `Ref.get()` on a class
no longer builds it: `name`, `pack`, `params`, `superClass`, `interfaces`,
`kind` and the other header fields come from the header; `fields`,
`statics`, `constructor`, `init` and `overrides` build the class when read.
`meta` is the declared metadata (`cl_header_meta`: before build macros and
inherited `@:autoBuild`) unless the class is the one being built by the
running build macro or the running expansion built it with the new
`Context.buildType(t)`; additions and removals still apply to the class.
The view no longer depends on whether something else happened to build the
class first (`tests/misc/eval/projects/buildType`). `-D macro-meta-check`
warns when a macro reads the declared metadata of a class that has build
macros. Libraries guard the call with `#if haxe6` (a new reserved define of
this compiler). In the corpus, two readers of build-added metadata needed
`Context.buildType`: hxsl's `loadShader` (`:src`, Heaps) and hide's
`hrt.shgraph.Macros.loadShader`; the JS output of Heaps and hide is
identical before and after.
`ClassField.expr()` on a field whose body is being typed (the field itself,
or a field on the typing stack) is a cycle error (`Cycle detected while
typing: a macro reads the body of copy, which is being typed`), instead of
returning `null`. The functions deciding these views belong to the API of the
running expansion (`compiler_api`), installed for the duration of each macro
call, so that a nested expansion (a build macro run by `Context.getType`)
does not leave its own view behind. Virtual file names of macro-defined
modules are now the module path alone (`<pack>/<name>`), without the
compilation step, so that warm and cold compilations report the same file.
Reading the fields (`fields`, `statics`, `constructor`, `init`,
`overrides`) of the class being built from its own build macro is an error
pointing to `Context.getBuildFields()` (`buildType/compile-local-fields-fail`).
`AbstractType`'s casts, operators, array access and resolve fields come from
the implementation class's fields: reading them builds that class instead of
returning whatever it holds so far. Neither change affects the corpus (same
JS output for Heaps and hide).
The `tests/incremental` cases `build-macro-cycle`, `meta-read-without-build`,
`macro-field-expr-cycle` and `define-type-two-owners` pass and are no longer
marked as known failures.

**Status: `sig` no longer demands `body`.** A field whose type is fully known
from its declaration (all argument and return types annotated, typed by a
literal default value, or a syntactic `Void` return) exposes that type
without typing its body: its `cf_type` is the declared type itself, and the
function typing its body is kept aside (`TFunctions.pending_bodies`). A first
version wrapped the type in a lazy state; code matching a type without
following it (`??` checking for `Null<T>`) then saw a different type depending
on whether the body had been typed, which `-D typer-shuffle` revealed.
The body is typed later (`PTypeField`), or on demand by
`TFunctions.force_field_body` where another field's body is needed while
typing: the default values of a super constructor's arguments, abstract
operator and array access methods (their `cf_expr` tells whether they are
being typed), the macro API's `ClassField.expr()`, display. Inline and
`@:generic` fields, whose bodies are needed by their callers, and fields of
the macro context keep the previous behaviour. This removes order-dependent
inference cycle errors: a constructor calling `addNodes` (inferred), which
calls `addNode` (fully typed), whose body calls `addNodes`, used to report a
cycle because reading `addNode` typed its body inside the inference of
`addNodes` (hxparse's `LexEngine`); the cycle errors of hxtemplo's
`Converter` disappear the same way.


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

**Status (first items).**
- Virtual file names of macro-defined modules are `<pack>/<name>`: the
  global counter and the compilation step are removed (a module path is
  defined at most once per context).
- `Compiler.include` sorts each directory listing.
- Monomorphs in messages are already numbered per message (print context).
- "Did you mean" suggestions only use the fields of a type or the types of a
  module, never the set of loaded modules.
- Resources are generated sorted by name in JS, Lua and HL (they were
  generated in hash table order, which depends on the order in which macros
  added them).
- A field whose typing reported an error is failed: the parts of its type
  its typing could not determine are bound to `Dynamic` (an error type), so
  that the fields using it do not report cascading errors, such as "not fully
  inferred" for a field returning `make().foo` when `make` failed
  (`tests/misc/eval/projects/failedFieldCascade`).
- Compiler messages are reported sorted by position (file, then offset),
  each message keeping the related messages that follow it
  (`MessageReporting.sort_by_position`, stable). The whole `tests/misc`
  suite and `tests/incremental` with `--strict-order` (messages compared
  without sorting) pass unchanged.
- The resources given to every generator are inserted in name order
  (`Common.to_gctx`), so no generator depends on the order in which macros
  added them.
- The remaining `Hashtbl` iterations only feed unordered tables or error
  reporting (now sorted by position).
- Moved to the phases they depend on: stored typed expression ids, local to
  the expansion key (phase 7: a stored expression can be kept in a macro
  static and returned by another expansion), and anonymous structure naming
  for hxb (with the hxb disk backup).

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

**Status: step 1, domain-local typing state.** The state that follows the
call stack of typing is local to the domain typing (`DomainLocal`,
`src/core/ds/domainLocal.ml`): the query stack, the stack of fields being
inferred and their error counts, the implicit cast and native unification
recursion stacks, the hook wrapping lazy type forcing, and the call
argument context. The settings that typing overrides for a while (the
error handler of speculative typing and macro calls, the disabled report
mode, the message capture of overload resolution) are no longer swapped in
the shared `Common.context`: they are bound to the context for the dynamic
extent of the computation in the current domain (`Common.push_error_ext`,
`Common.error_ext`, `report_mode`, `message_capture`). The counters for
stored typed expressions and generic short names are atomic, the set of
reported inference cycles is protected by a mutex. No change of behavior
(typing still runs on one domain).

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

**Status: done (items 1, 2, 3 and 5).** The preprocessor records the defines
it reads, with the value seen (`pd_define_reads` in the parse result): the
`#if`/`#elseif` conditions, and `macro`, now read only when a reification
needs it. The server keeps the parse results of every context
(`CompilationCache.cache#find_shared_parse`), keyed by file and modification
time (the VFS gives a stable modification time per compilation), several per
file; a context parses a file only when no result has the same values for the
defines it read, then also keeps it in its own cache. A file without
conditional compilation is parsed once for the macro context and every
target. Display files are never shared. `tests/server` (`testSharedParse`,
`testSharedParseConditional`) checks both cases. Item 4 comes with phase 4.

Exit criterion: the pass scheduler is removed (`typerPass.ml`, `delay`,
`flush_pass`, `delay_if_mono`, `make_lazy`'s forcing, `build_state`), all
tests pass, compilation output is unchanged compared with the pass-based
compiler on the whole corpus (modulo the documented phase 1 and cycle errors),
and parallel typing produces the same output as sequential typing.

**Status of the exit criterion.** The pass scheduler is gone: `typerPass.ml`
is `programStage.ml` (the stages of `program()`: `StageModules`,
`StageTypes`, `StageMembers`, `StageBodies`, `StageConstraints`,
`StageChecks`, `StageFinal`), `delay`/`delay_late`/`flush_pass` are
`schedule`/`schedule_late`/`run_stages` over FIFO stage queues, the typer
context records the stage it was created for (`ctx.stage`), `delay_if_mono`
and `make_pass` are removed, and `make_lazy`'s forcing is a unit of the checks
stage. `build_state` stays as the state of the `members` query of a class
(`cl_build`). The output of the corpus (Heaps, hide) is unchanged except for
the documented order of domkit handlers. Parallel typing (§3.5) is not done.

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

**Status (first version, `src/core/vfs.ml`).** On macOS, directories are revalidated on every
compilation instead of being watched: FSEvents (used by libuv) delivers events
asynchronously, and a change made a second before a compilation was missed
(`Issue13051` server test on the macOS CI). Implemented:
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

- **Persistence (hxb on disk).** Storage is in memory only. hxb is a
  **separate, parallel backup task**: after a compilation, the state is
  saved **asynchronously** to a cache file (`cache.hxb` by default, or
  `-D hxb-cache=<file>`), without ever blocking compilation. It only serves
  to **save and reload faster between compilations** when the compiler does
  not run as a server, or when the dedicated server was stopped: the cache
  file is loaded at startup instead of compiling cold. To reload into a
  state that is ready for incremental compilation, the file has to hold the
  incremental data as well (entity keys, fingerprints, dependency graph, VFS
  state), not only the typed modules. Whether it pays off must be measured
  on large codebases: reload time against a cold compilation. (The former
  hxb cache, which kept hxb chunks in the server's memory and persisted
  nothing, is removed.)

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
