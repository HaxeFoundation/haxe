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
| Macros | Automatic tracing of compiler API reads; **volatility is explicit** (`Context.setVolatile(?check)`), like file dependencies; volatile expansions are re-executed with early cutoff on their output. Console output (`Sys.println`, `trace`) is printed immediately, in no guaranteed order, and not replayed from the cache. **Every macro expansion starts from the base of the macro statics** (what their initializers and the initialization macros left), its changes undone when it ends; `@:persistent` statics are **caches** (pure memoization, which the compiler may empty at any time); data attached to types uses **`haxe.macro.TypeCache<T>`**, given by the build of the type or computed by a pure function of it. File dependencies are **explicit**: only files declared with `registerModuleDependency` are tracked (and watched, wherever they are); reading or writing other files is the macro's responsibility. |
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
| Parallelism | **Every compilation operation runs in parallel**, not only typing: file system events and hashing, parsing, every typing query, macro expansions, fingerprints, post-processing, global passes, code generation and output writing (§4b). The query engine is the scheduler, on a persistent pool of OCaml 5 domains (`src/context/parallel.ml`). **Enabled by default** with one domain per available core; typing uses at most 4 of them by default (`-D parallel-typing=<n>`, `0` types on one domain, see phase 3.5), `-D disable-parallelism` disables all of it. Output must be identical whatever the number of domains and the scheduling order (checked in CI). |
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
reference that does not point to the object its module currently holds. The
check is meaningful because the server keeps its modules in memory (the
storage chosen by the plan); the former hxb cache, which decoded the cached
modules on every compilation, and its code are removed. `tests/incremental` can run in that mode with
`--args "-D incremental-check-refs"`: no stale reference
is reported. The one pre-existing warm/cold difference of that mode
(`macros/add-resource` on js: DCE dropped `js.Boot` and every feature-dependent
`__name__` on a module reused from memory) is fixed by the first step of
phase 2, item 3 (see "DCE markers" there).

**Warm/cold fuzzing** (`tests/fuzz/fuzz.py`, see `CLAUDE.md`). Random edits
of a project (comments, literals, added fields, inferred return values), each
followed by a compilation through the server and one from scratch, compared
(exit code, messages, output). The cold compilations vary the typing order
(`-D typer-shuffle`) and the number of domains, the server uses a random
number of domains for the run. Part of the edits (`--errors`) introduce an
error at a given level (syntax, unknown type in a signature, field
initialization, method body, unknown identifier, argument of a macro, error
raised by a macro), undone after the compilation: the errors must be the same,
and the output too once the error is undone. With `--mem`, the memory kept
alive by the server (`server/gcCompact`, field `live`) is measured after each
compilation, and a growth is reported as a leak. Run on Heaps (JS and HL) and
hide, it found the bugs fixed in phase 6 ("Status: fuzzing", below).

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
  `neko.Lib.load(...)`): error like any other unresolved type. Today its
  monomorph stays open and is bound by its first use (often to a function
  type); in Haxe 6 it needs an annotation (`: Dynamic`). `neko.Lib.load` and
  `loadLazy` return `haxe.Constraints.Function` instead of `Dynamic`: a
  callable type, which converts to any function type (`@:to`) and has no
  field, so the Neko natives they load need no annotation.
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

**Rules applied to typing.** Four rules that make a field's signature known
from its declaration are applied by the typer of this branch (not only
measured): a function without `return <expr>` and without return type hint
has a `Void` return type (syntactic `Void`), an argument without hint
whose default value is a literal determining its whole type (`""`, `0`,
`1.5`, `false`, `-1`) has that literal's type, and an argument without hint
of a method overriding or implementing another one has the declared type of
the same argument there (its hint, literal default value or, recursively,
what its own parent declares; never by typing the parent's body, the table
`Typecore.declared_args`) (`setup_args_ret` in
`src/typing/typeloadFields.ml`, tests in
`tests/misc/eval/projects/haxe6FieldRules` and
`tests/incremental/cases/haxe6/override-args-inherited.hxtest`). At the end of the typing of a
method, the return type of a function argument (a callback) that nothing
determined and that appears nowhere else in its signature is `Void`: the
method only calls it for its effect (`f()`, `onDone(result)`; test
`tests/incremental/cases/haxe6/callback-return-void.hxtest`). It removes
most of the annotations of callbacks, and those of the result callbacks that
hxbit adds to the RPCs. A `@:from` cast function without return type hint (of an
abstract without type parameters) returns the abstract, also without typing
its body, and a `switch` over an enum abstract reads its values only: both
made false inference cycles between the inferred methods of an abstract (the
search of its casts or of its values read the types of all its fields; test
`tests/incremental/cases/haxe6/abstract-fields-no-false-cycle.hxtest`). Reading a field
whose type is being inferred is not an inference cycle when that type is
already fully determined at that point (a first `return` set the return type
before the recursive call): what is bound does not change, the reader sees the
final type (test `tests/incremental/cases/haxe6/recursion-type-already-known.hxtest`;
castle: `Sheet.objToString` / `colToString`, `Database.valToString`). A variable initialized with a
`Dynamic` expression is not given the type `Dynamic`: it is an unresolved
type like the others (a first version of this branch made it `Dynamic`). In
`+`, an operand of unknown type next to a `Float` or `Dynamic` operand is an
`Int`, as it already was for `-`, `*`, `/` and `%` (`Float + unknown` and
`Dynamic + unknown` left it unbound, which Haxe 6 reports as a field type not
fully inferred); `String + unknown` stays open. An argument without hint only
used as `x + Math.cos(a) * r` is then an `Int` (Heaps: the local function
`corner` of `h2d.Graphics`, annotated). The result of a recursive call of a
method whose return type is inferred is unknown in its own body: added to a
`String`, it is converted with `Std.string`, which the inferred `String`
does not remove afterwards (`hxsl.Tools.toString` keeps its annotation). The unit tests (js, macro, hl),
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
ones (the Neko natives loaded with `neko.Lib.load` need none, see above), plus the return types breaking the inference cycles of
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

**Status: step 2, signatures before bodies.** `program()` has a stage for
the fields whose type is inferred from their body (`StageSigs`, before
`StageBodies`): a class adds the typing of those fields (and of inline
fields, whose body is read by their callers) to it, and the typing of the
bodies of its fields whose type is declared to the bodies stage. All the
signatures known when the bodies stage starts are computed, so a body whose
field has a declared type can only demand other bodies, never change the
type of another field. Generic fields stay with the bodies (their instances
are created by the bodies that use them; typing one before its users made a
macro-context instance of `Pull11784` fail). The documentation inherited by
fields is built in the signatures stage, before any display request typed
there needs it.

**Status: step 3, the typing lock.** `TypingLock` (`src/core/typingLock.ml`)
is a reentrant lock, only taken while bodies are typed in parallel. It
protects the operations of body typing that change shared state: loading a
module, demanding the header or the members of a type (no longer since step
12: a class is built by the domain that claims it) (and completing a
module read from `.hxb`, resolving `@:using`), calling a macro, creating a
generic instance or a coroutine class, inlining an extern into its
implementation class, forcing a lazy type, scheduling `program()` units,
reporting messages (the default handlers and the module and diagnostics
messages), features, the overload cache, stored typed expressions, the
`CfStructurallyUsed` flags set by unification and the temporary closing of
constant structures. The bodies of fields are claimed: `force_field_body`
types a pending body, returns when the current domain is typing it, and
waits (releasing the lock) when another domain is
(`TFunctions.body_started`, `body_done`).

**Status: step 4, parallel bodies (`-D parallel-typing`, experimental).**
The bodies stage has parallel units: the typing of the bodies of the fields
with a declared type of each class, grouped by module (the classes of a
module share its typer and import resolution, so a module is typed by one
domain, in order). When the bodies stage reaches them, `run_stages` runs the
modules on the domains of the compilation's pool, with the typing lock
active; units added meanwhile run afterwards, and nested stage runs (from a
domain, or while typing on several domains) run sequentially under the lock.
A domain claims each body it types; a body demanded by another domain is
typed by it if still pending, or waited for. Macros called from another
domain get their own evaluation state (`EvalMain.ensure_thread_eval`); they
ran under the lock (since step 12 only their API calls do). Not enabled in display requests, in the macro context
and with `-D fail-fast`. Reading what is final needs no lock: a built class
(`TAccess.built`), a computed header, a completed `.hxb` module, a created
module (`m_ready`), a known missing module. The claims of bodies use the
mutex of the claims (`Claims.mutex`, shared with the lazy types and the
class builds), and a domain waiting for a body releases the typing lock
(`TypingLock.without`). The stage queues have their own mutex as well
(`Typecore.program_mutex`): adding units, the most frequent operation under
the lock while bodies are typed (late argument checks, constraint checks,
lazy type forcing), no longer waits for the typing lock. On Heaps, the
domains then wait for the typing lock about 400 times in a compilation.
Found and fixed on the way:
- reading a class's build closure before taking the lock ran a build that
  another domain had just done (`Duplicate class field declaration`);
- a module was visible to other domains before its types were created;
- coroutine code was named after global variable ids
  (`_hx_hoisted<v_id>`, `setStackItem` ids of local functions) and its
  hoisted fields were in hash table order: names now come from a numbering
  local to each coroutine, in order of appearance, and the id of a local
  function is its position (the output no longer depends on how many
  variables were allocated before).
The generated JS of `tests/unit`, Heaps and hide is identical to sequential
typing; `tests/unit` (macro) and `tests/misc` pass with it, and the macro CI
job runs `tests/unit` with it. Measured on 4 cores: `tests/unit` (JS) 3.2 s
→ 3.0 s, Heaps 5.1 s → 4.6 s, hide 18.5 s → 17.5 s. The gain is limited by
what still runs under the lock while bodies are typed (classes built and
modules loaded on demand, macros) and by the stages that stay sequential
(structure, signatures, macros, filters, generation).

**Status: step 5, signatures in parallel.** The signatures stage has
parallel units too (the fields of each class whose type is inferred, by
module). Lazy types are claimed like bodies instead of being forced under the
typing lock (`TFunctions.lazy_type_parallel`): the domain that claims a lazy
forces it, the others wait for it. A domain never waits when the domain it
would wait for waits (transitively) for it (`TFunctions.Claims`): it then sees
the placeholder of what is being computed, as a cycle on one domain does. A
cross-module inference cycle split between two domains is therefore not
reported as an inference cycle in parallel mode. Messages, features, caches and the flags set
by unification use a leaf mutex instead of the typing lock
(`TypingLock.protect_leaf`), class path lookups have their own mutex, and a
module that no class path, cache, `.hxb` library or extern loader can provide
is reported missing without the lock.
Measured (4 cores, sequential → parallel, identical output): Heaps 7.1 s →
5.5 s; hide (main compilation) 19.8 s → 15.4–16.4 s, its bodies stage 7.4 s
→ 3.3 s. What remains under the lock is mostly macro calls and the first
lookups of modules; the class builds stage (`StageTypes`, 1.2 s on Heaps,
2.4 s on hide) is still sequential.

**Status: step 6, the order of effects.** Typing on several domains must give
the output of one domain, also when macros keep state between calls (domkit
numbers its properties and registers its components in the order its build
macros run). What has such effects is serialized and ordered:
- **Classes are built under the typing lock**, never in parallel (building
  them in parallel split build cycles between domains, with an outcome
  depending on timing). A build can still release the lock while it waits for
  another domain: a domain that needs a class being built by another one waits
  for the build to end (`build_started`, `build_done`) instead of reading it
  partially. The class builds stage (`StageTypes`) stays sequential. (Since
  step 7 this stage has parallel units, and since step 12 every class is built
  by the domain that claims it, without the typing lock:
  `TAccess.with_unlocked_claim`.)
- **The macros run on one domain at a time** (`Claims.with_macro_claim`), even
  while the domain running them waits for another one; the claim is only
  taken by releasing the typing lock when another domain holds it. An
  expression or build macro only takes it at its first access to what the
  macros share (step 10, 11); its first call loads it with the claim.
- **The tasks of a parallel batch are started in their order** (one worker per
  domain takes the next task, `Claims.next_task`); their effects do not wait
  for the tasks before them (step 11): the output must not depend on the
  order in which they happen, which `-D typer-shuffle` checks on one domain.
- **Waits form a graph** (`src/core/claims.ml`): a domain that would close a
  cycle of waiting domains lets one of them go on, choosing in this order the
  macro claim (taken over by the domain let through, so macros still run one at
  a time), a value (a lazy type, a body, a constructor: the placeholder is
  seen, as in a cycle on one domain), and a class being built, the one whose
  build started first.
- The modules are parsed outside of the lock before being created
  (`TypeloadParse.preparse`). The parallel mode is disabled again in the macro
  context.
Found and fixed on the way: a batch started from a lazy forced inside a task
(batches only start outside of units, `Typecore.unit_depth`); the monomorphs
of the arguments and return types of fields kept in the expression state of
their module, shared by the domains typing it (each field gets its own); the
table of pending constructors (claimed like bodies); the conditional
compilation state of the parser shared by domains parsing in parallel; a
genericBuild macro run twice while the lock was released (`Cannot redefine
module`); two domains let through the macro claim at once (domkit handlers
missing); the search of a cycle looping on a cycle without the current
domain.
Measured (4 cores, sequential → parallel): the generated JS of Heaps and hide
is identical in every run (hide: 10 out of 10, previously about 1 run in 6
differed or failed), `tests/unit` (macro) passes 40 times out of 40; Heaps
5.0 s → 4.6–4.9 s, hide 19 s → 15–16.5 s. Most of the typing time left on
one domain is in macros (3.7 s on hide, 1.5 s on Heaps), which have to keep
their order.

**Status: step 7, cheaper order, parallel class builds.**
- `--times` reports the wall time of each parallel batch (`typing.parallel.<stage>`)
  and, below the timers, the time the domains waited (summed over them) for the
  typing lock, for each kind of claim and for the order of the tasks (by what
  waited: a build, a macro, a module), with the cycles broken by kind.
- **Each task of a batch has its own queues** (`Typecore.task_queues`, only
  for the `program` of the batch, not for the one of the macro context): the
  units it adds to earlier stages run after each of its units, the others are
  added to `program` in the order of the tasks once the batch is done. Before,
  another task could run them, out of order (a domkit component built after
  the others), and most of the cycles broken (500 to 5000 per compilation of
  hide) came from there: 2 are left.
- **Only builds that run build macros wait for the order of the tasks**
  (no order since step 11, no typing lock since step 12)
  (`TAccess.has_build_macros`: the class's own `@:build`, or an `@:autoBuild`
  of a super class or interface), with the macro claim, under the typing lock.
- **The class builds stage has parallel units** (by module): a class without
  build macros is built without the typing lock by the domain that claims it
  (`TAccess.with_unlocked_build`), the others wait for the build to end.
- The worker domains of the pool get a 32 MB minor heap (16 MB since then: as
  fast on SpaceCraft, about 0.5 GB less memory on 22 hardware threads)
  (`Parallel.set_worker_minor_heap`, `HAXE_WORKER_MINOR_MB`): a new domain gets
  the default size whatever its creator uses, and the minor collections stop
  every domain. hide: about 19.4 s → 17.3 s.
- genjs generates the types in parallel, each on its own buffer, then puts the
  code in order, creating the packages there (without a source map only: a
  replay of the source map in order cost more than it saved; since then each
  type gets its own source map, whose mappings are transcoded in parallel,
  step 9, and types are generated in parallel with a source map too). Heaps: 0.23 s →
  0.11 s.
What stays: the macros of field bodies wait for the order of the tasks (on
hide about 9 s summed over the domains, 230 waits: `EditContext2.build` and
`hxsl.Macros.buildShader` are 2.4 s of macro execution), and run one at a time
anyway, their interpreter state being shared; the parallel filters and
analyzer scale by about 2 on 4 domains, limited by the minor collections that
stop every domain and by the allocation of the major heap (sampled stacks).

**Status: step 8, macros out of order.** An expression macro already loaded
(its first call loads and types its module, in order) starts without waiting
for the tasks before its own (`Claims.speculate`): it waits for them at its
first access to what the macros share, releasing the claim of the macros
meanwhile so that the macros before it run, then goes on in order with its
macro API state back (superseded by step 11: there is no order to wait
for, the macro takes the claim with `Claims.end_speculation`). What is shared
(`EvalValue.on_shared_state`): the static variables of the interpreter (read
or written, directly or by reflection), the macro API except the functions
that read the compilation or type (`EvalMain.speculative_api`: typing waits
for the order itself when it has an effect that follows it), changing
metadata or excluding a type, the output, files, processes, the environment,
random numbers and resources of the standard library, and a module that is
not found (a macro before it may define it). Waits whose condition already
holds, or let through, are not part of a cycle any more: they made most of
the cycles broken once macros could go further (none are left on hide).
On hide 1416 of the 1425 macros started out of order are done without
waiting, the bodies stage goes from 5.3–6.7 s to 2.7–3.7 s and the main
compilation from 17.4–21 s to 14.6–16.2 s (sequential: 17.9–18.8 s); the
output stays identical. A macro that reads typing state that an expression
macro before it changes through the API without the change being one of the
effects above (reading metadata that it adds, for example) would read it
early: build macros are not affected (builds that run macros wait for the
order before they start).

Found by sampling the stacks of a compilation of hide on one domain (gdb):
- string interpolation parsed each `${...}` group after as many spaces as its
  position in the file, lexing the start of the file again for every group:
  the lexer now starts at that offset (`ParserEntry.parse_expr_string`).
  hide: typing 5.3 s → 2.6 s, its main compilation 17–19 s → about 10 s;
- JS source maps looked the file of each expression up in a locked table,
  built an array for each base64 digit, compared positions polymorphically and
  found newlines with an exception: generation with `-debug` 1.55 s → 0.6 s.
What is left is spread out: the GC (about 30 %: marking, sweeping, the
initialization of new pools of the major heap, promotion), the filters and
the analyzer, typing and macros. Typing being shorter, parallel typing now
saves 0.3–0.5 s on hide and about 0.1 s on Heaps.

**Status: step 9, on by default.** Parallel typing is the default,
`-D parallel-typing=0` types on one domain (the macro CI job runs
`tests/unit` that way). Found by running every test suite in parallel:
- the evaluation state of the macro interpreter is per thread: a context
  created by a domain typing in parallel and read from another thread (the
  main one, flushing the macro context of an `--interp` compilation) failed
  with `Thread_local_storage.Not_set`; it is now created on first use;
- the `using` of a module were resolved by a late unit of the task that
  loaded it, and a body of the module typed meanwhile by another domain saw
  none (a static extension of hxcoro not found, about one compilation of
  `tests/server` in four): a domain about to read them resolves them, the
  first one done publishes them.
Also: the modules of `Compiler.include` are parsed in parallel before being
included in order (`preparse_modules`), the caches of the server (also used
without server) being read and written under the leaf lock; the HL
optimization runs on the pool of the compilation (0.47 s → 0.26 s on Heaps);
the mappings of the JS fragments are transcoded in parallel (hide: merge
0.38 s → 0.07 s); HL/C names its structural types from a canonical
description and lists its hashes in string order (both depended on the
scheduling). Measured cold (4 cores, median of 5): Heaps JS 4.8 s (Haxe 5)
→ 3.8 s, Heaps HL 6.0 s → 4.8–5.0 s, hide 16–16.7 s → 8.8 s, the output
identical in every run. On hide the domains still wait mostly for the order
of the class builds that run macros, and the macros themselves (2.4 s)
stay on one domain at a time.

**Status: step 10, expression macros on several domains (§4b, item 3).**
The state of the macro interpreter that an expansion changes is local to the
domain running it: the compiler API of the expansion (`curapi`, the functions
of the encoders), and the copy of the base of the static variables that its
expansions change and undo, kept by domain slot on their prototype
(`get_static`, `set_static`; the `@:persistent` slots and the statics of the
interpreted program stay shared). The tables the
macros share at run time (the names of the hashes, the prototypes of
anonymous objects, the fake prototypes of the encoders) have locks. An
expression macro started out of the order of the tasks (speculating) then
runs without the claim of the macros, its interpretation without the typing
lock, which every function of the macro API takes (except those of
`EvalMain.lock_free_api`, step 12): other domains type and
run their own macros meanwhile; it still waits for the order of the tasks at
its first access to what the macros share. hide: the bodies stage 1.0 s →
0.7 s, the output identical in every run; cold (4 cores, median of 5): Heaps
JS 4.8 s (Haxe 5) → 3.9 s, Heaps HL 6.1 s → 4.7 s, hide 14–16.7 s → 8.4 s.
What is left: the classes whose build runs build macros are built in the
order of the tasks (the domains wait for it about 5 s summed on hide, in the
class builds stage); running their macros speculatively needs them to run
before the build is marked started, or the build to be given up when another
domain needs the class.
Found on the way: the array access of a class (`implements ArrayAccess<T>`)
was read without demanding its header (`TAccess.Header.array_access`).

**Status: step 11, no order between the tasks.** The output must not depend
on the order in which the effects of the tasks happen, so the tasks no longer
wait for the ones before them (`Claims.in_order` is gone): what remains is
mutual exclusion. A class whose build runs build macros is built by the
domain that claims it, like the others; an expression or build macro already
loaded runs without the claim of the macros, its interpretation without the
typing lock, and takes the claim at its first access to what the macros share
(`Claims.end_speculation`), keeping it until it is done; a macro that loads
its module runs with the claim: the first call of a macro loads it with
the claim, then runs speculating. Reading files (`sys.FileSystem.exists`,
`stat`, `readDirectory`..., `sys.io.File.getContent` / `getBytes`) or the
environment (`Sys.getEnv`, `Sys.time`...) is not an access to what the macros
share; writing files, running processes and printing are. A module not
found while speculating is looked up again with the claim (a macro holding it
may define it). Checked with `-D typer-shuffle` on one domain (hide, Heaps JS
and HL: identical for every seed) and by comparing parallel runs with the
sequential output. What depended on the order: domkit emitted the handlers of
a component in the order of the property ids, which are given by the first
component built that uses the property (now emitted by name); the server test
`CsSafeTypeBuilding` expected `Foo<Bar>` to be generated from Bar rather than
from Baz, both using it (either is accepted). hide: 578 of the 604 build
macros are done without the claim, the class builds stage waits 0.01 s for
it instead of 1.9 s. Cold (4 cores, median of 5, Haxe 5 → 6): Heaps JS 4.7 s
→ 3.6 s, Heaps HL 6.2 s → 4.6 s, hide 16.2 s → 7.0 s (8.4 s at step 10).

**Status: step 12, classes and macros without the typing lock.**
- **Headers and builds under the claim of the class only.** The header of a
  class (super class, interfaces, declared metadata) and its build, with or
  without build macros, are computed by the domain that claims the class
  (`TAccess.with_unlocked_claim`), without the typing lock; the other domains
  wait for them to end. The macro API reads a header through
  `TAccess.Header.demand` rather than calling `cl_build_header` directly: a
  macro read the header another domain was computing (hxbit: "Unsupported
  serializable type Array<ent.Ship>", about one compilation in eight on
  SpaceCraft).
- **Every macro runs without the typing lock** while typing on several
  domains, not only a speculating expression or build macro
  (`MacroContext.do_call_macro`): other domains type meanwhile. What the macros
  share is still ordered by the claim of the macros. What stays under the lock
  is around the interpretation (`TyperEntry.do_macro`): loading the macro,
  typing and encoding its arguments, decoding its result, and the calls of the
  macro API.
- **A macro reading the type of a field that another domain infers waits for
  it** (`MacroApi.encode_cfield`), instead of getting the lazy type: on one
  domain the macro would not run meanwhile, and a macro that does not follow
  `TLazy` saw a type it does not handle (hide's prefab macros generated
  `type: null`).
- **The API functions that read nothing of the compilation**
  (`EvalMain.lock_free_api`: `init_macros_done`, `make_expr`, `current_pos`,
  `get_pos_infos`, `make_position`) do not take the typing lock: about 150000
  calls on SpaceCraft, made by the macros, the critical path of typing.
Measured on SpaceCraft (HL, cold), for the lock-free API functions: 22 typing domains 22.5 s → 19.6 s, 4
domains 16.0 s → 15.8 s; with the macros run without the typing lock, 60
parallel builds out of 60 are identical to the sequential one.
What still takes the typing lock: the calls of the macro API and what surrounds
the interpretation of a macro (above), creating a module loaded on demand (its
parsing is done before, outside of the lock), completing a module read from
`.hxb`, resolving the `using` of a module, creating a `@:generic` instance or
typing a generic function, turning a function into a coroutine, inlining an
extern into its implementation class, the temporary removal of the constant
flag of structures (`Fields.remove_constant_flag`), and the units run by a
nested stage run (`Typecore.run_stages_upto` from a domain typing in
parallel).

**Status: step 13, less serialized by the typing lock and the claim of the
macros.** Measured on SpaceCraft (4 domains), the typing lock was held 1.0 s
and the claim 1.6 s of the 2.4 s of the class builds stage: typing was
serialized by one or the other. `--times` now reports, per parallel stage,
how long each was held and for what (`held: typing lock: <site>`, `held:
macros: <reason>`, including the access that ended a speculation), and
`HAXE_TYPING_TRACE=<file>` writes the waits, claims and builds of every
domain with their time (`TypingTrace`), from which a timeline of a stage
shows what serializes it.
- **Macro calls without the typing lock.** `MacroContext.type_macro` no
  longer runs under the typing lock. What it types in the macro context (whose
  typer is not domain-safe) is typed with the claim of the macros: loading a
  macro (typing its module, flushing the macro context, initializing its
  classes; the stage runs of the macro context take no typing lock either), and
  what a macro needs besides its arguments, computed once per macro and mode
  (`macro_info`: its signature checked against the mode, how each argument is
  passed, the type of `Field`). Arguments passed as expressions, constants of
  a basic type and omitted optional arguments are encoded without typing them
  (`untyped_args`, what typing them gives); other arguments are typed with the
  claim. The table of loaded macros has a mutex.
- **More of the macro API without the typing lock.** The functions that only
  read the compilation as body typing does (parsing, following and printing
  types, `toComplexType`, the defines, the local type, method, imports and
  variables of the expansion) join `EvalMain.lock_free_api`.
- **A module not found while speculating** is looked up again once the macro
  holding the claim is done (a temporary claim), and the speculation goes on:
  most of these are lookups relative to the package (`ent.Array`).
- **Frozen persistent state.** At the start of each parallel batch, when no
  macro runs, and after each macro load (with the claim), the `@:persistent`
  static variables are frozen (`EvalValue.freeze_persistent`): each variable,
  with the arrays, objects, instances and maps it holds, forms a group whose
  number marks them (`amark`, `omark`, `imark`). A variable holding something
  whose changes cannot be seen (a function, which may have captured objects, a
  vector, bytes, a buffer...) is not frozen. A speculating macro reading a
  frozen variable goes on speculating, registered as a reader of its group
  (`Claims.read_frozen`). Writing the variable, or changing one of the marked
  objects (the write barrier of the array operations, of field writes,
  `Reflect`, and the map operations), ends the speculation of the writer, then
  thaws the group (until the next freeze), which invalidates its speculating
  readers (`Claims.thaw`). A writer never waits: an invalidated macro has no
  effect yet, it is stopped at its next access to what the macros share or API
  call (`Claims.Invalidated`), or when it is done, and runs again with the
  claim. Only the macro that started a speculation can read a frozen group or
  be stopped: a macro nested in it (run by the typing of an API call) has
  effects that stay (the classes it builds...); it ends the speculation
  instead, and the outer macro runs again when it is done if it was
  invalidated.
- **Speculations are validated.** A speculation that ends after another
  domain released a claim taken to change what the macros share (a macro, a
  load) may have read what it changed: a type not defined yet, which it would
  then define again. It runs again with the claim (counted by `--times`,
  `macros run again`). Generic build macros now speculate too (a
  check-then-define macro that loses the race runs again).
- **Nested macros are validated too.** A macro nested in a speculation (run
  by the typing of an API call of the macro that started it) may read what
  another domain changes before the claim is taken, then define a type a
  second time. Each one started while the domain speculated is a frame
  (`Claims.frame`): it is checked at the first access to what the macros share
  made by its own code (not by what an API call it made runs: the
  initializers of a macro load, the typing; `Claims.api_depth`), or when it
  ends if a macro nested in it took the claim, and if another domain changed
  what the macros share since it started, it runs again alone, with the claim
  (`Claims.Rerun`, raised and caught within its own interpretation: it never
  crosses the typing running it). Its callers are checked the same way once
  it is done, the macro that started the speculation as before. A nested
  macro that ends without accessing what the macros share is not run again,
  like the outer one (`cases/macros/nested-macro-define-race.hxtest`).
Found on the way: an invalidation raised from a macro nested in the typing of
another (a generic build macro forced by `Context.typeof`) left the lazy type it
was building unresolved (hxbit schemas with `null` field types): nested macros
are never stopped through the typing that runs them, nor is the macro that
started a speculation while an API call it made runs.
Measured (4 domains, median of 10, parallel typing stages before → after):
a project of 300 hxbit classes (a third `NetworkSerializable`, the pattern of
SpaceCraft) 1.31 s → 0.67 s (bodies 0.77 s → 0.24 s, the whole build 5.2 s →
2.7 s): every speculating macro now ends without the claim of the macros
(1708 of 9705 before), the claim is held 2.0 s → 0 s in the bodies stage, and
73 to 104 generic build macros run again. Heaps (`-D apicheck`) JS 0.71 s →
0.68 s, HL 0.71 s → 0.70 s, its samples (one HL build) 1.57 s → 1.48 s: the
typing lock is held 0.34 s → 0.06 s in the class builds stage, which stays
bounded by the macro loads (the claim, on the critical path: every class
needing a macro waits for it). The output stays identical to the sequential
typing (40 runs out of 40 on Heaps HL and on the hxbit project, `-D
typer-shuffle` for 4 seeds, 1, 2, 8 and 16 domains).
Also fixed since: a `@:genericBuild` or `MacroType` expansion changed the
class and expression states of the context it ran on, and another expansion
running meanwhile on the same context read its local type or returned its
result (hxbit's `Proxy<T>` of different fields swapped on SpaceCraft): each
expansion runs on a copy of the context with its own states. A cycle of value
waits between domains where one of them waits for the final type of a field
whose body it is typing itself (what a single domain reads in the same
cycle) is broken by letting that wait through first (`Claims.wait ~safe`).
What serializes typing now: loading a macro (typing its module in the macro
context, one domain at a time: 0.7 s of the 1 s class builds stage of the
Heaps samples is `hxd.res.FileTree.build`), and the macro API functions that
type (`typeof`, `getType`...) or change the compilation, still under the
typing lock.

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
the documented order of domkit handlers, now sorted by domkit itself (the
output no longer depends on the typing order). Parallel typing (§3.5) is done
and on by default.

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

**Status: done (items 1–4), computed on demand.** `src/core/tFingerprint.ml`
hashes into two 63-bit multiplicative lanes, without serializing: module
types by path, type parameters by host and class path, local variables by
order of declaration, unbound monomorphs by order of appearance, structure
fields sorted by name (cycles through monomorphs cut by a back reference),
statics of classes, enums and abstracts by path; parsed data (metadata,
declarations) by a walk over its blocks that recognizes positions
(`Pos.of_obj`). Interface fingerprints exclude positions; body and `Decls`
fingerprints include them relative to the position of their declaration, so
that they do not change when it moves (anchors keep a delta only: a base in
the anchor would make positions with the same offsets in two declarations
compare as different). `TypeHeader` (path, kind, flags, parameters with
constraints, super class, interfaces, `@:using`, `implements Dynamic`, array
access, declared metadata, typedef target, abstract underlying type),
`TypeMembers` (complete metadata, members in order with kind, flags and
overload count, constructor, `__init__`; enum constructors with their type;
abstract implementation, `from`/`to`, operators, array and resolve fields),
`FieldSig`, `FieldInlineBody` (inline, macro and generic fields, abstract
casts and operators) and `FieldBody` are computed once typing is done, in one
pass: the hash forces nothing, so it cannot change the order of typing and
is the same in parallel. `Decls` fingerprints are computed when a file is
parsed (typing changes parsed declarations, the values of enum abstracts,
and the contexts share them), per field, per type without its fields and for
the imports. They are only computed with `-D dump-fingerprints=<file>`, which
writes them sorted (about 0.6 s on Heaps, 83 000 nodes); the server will
compute them per node (phase 6). Checks: two cold compilations of Heaps, and
a parallel one, give the same 83 000 fingerprints; `tests/incremental` cases
can expect a fingerprint to change or stay the same from one step to the next
(`@expect-fingerprint-same <node> <key>`, `@expect-fingerprint-changed`,
on the cold compilation), and `cases/fingerprints/` covers moved fields,
body, inline body, signature, member, interface, typedef, enum, abstract,
metadata and access changes.

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

**Status: done, recorded on demand (`-D dump-fingerprints`, until the server
uses it in phase 6).** `src/core/depRecord.ml` holds the current consumer per
domain: the header and members queries of a type (`typeloadModule.ml`) and the
signature and body queries of a field (`typeloadFields.ml`) run under
`DepRecord.with_consumer`, which keeps the set of what they read, deduplicated.
The accessors record it: `Header.demand` and the typedef expansions
(`apply_typedef`, unification) record `DHeader`; `Members.demand`, enum
constructors and abstracts record `DMembers`; `find_field`, `find_static` and
the constructor record `DSig`; inlining (`calls.ml`, `AccInline`) records
`DInline`; `load_module` records `DLookup` (failed lookups included); macro
calls record their expansion (phase 7 will make it a node). Implicit
dependencies are added when the dump is written: the `Decls` of the
declaration and the imports of its file, the module's types, the define
signature, and the position of the declaration when the body uses line
numbers (`PosInfos`). The members of a type depend on the list of its fields
and on the **declared signature** of each field (its declaration without the
body of a function whose return type is declared, nor the value of a variable
whose type is declared), not on their bodies: the signatures the members
declare are computed with them. The messages reported while a node is computed are a
node of their own (`<node> messages`), so that they can be replayed.

The fingerprints of bodies hash the positions inside their declaration
relative to it, and only the file of the positions outside of it (the code
inlined from other declarations): these positions move with the anchor of
their declaration, without retyping.

Verification (`tests/incremental --check-deps`, `Replay.hx --check-deps`): from
one step to the next, a node whose fingerprint changed while none of the
nodes it read did is a missing dependency. The incremental cases pass
(`cases/dependencies/` covers structural unification, super class signatures,
static extensions, forward and `from` abstracts, underlying types, enum
arguments, `@:allow`, type parameter constraints, interface checks, moved
inline functions and added or removed fields), and so does the replay of 20
Heaps commits. A node reading itself is not a dependency (it would always
count as changed and hide a missing one).

Cost: recording takes about 0.3 s of the 1.2 s of typing Heaps (keys are
built as strings); phase 6 records it in every server compilation and keys
it by entity.

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
7. **Initialization macros** (`--macro`) are always executed **first**, so
   that the configuration they put in `@:persistent` caches is rebuilt on
   every compilation (phase 7, item 4). **Global callbacks** (`onAfterTyping`, `onGenerate`) and
   `program()` stage reductions are always executed **last**, with early
   cutoff on what they produce.

8. **File change detection and virtual file system**: see below. The server
   never scans the project on each compilation, and resolving a path never
   makes a system call.

**Status: red/green validation of cached modules (first step).** The server
keeps a query graph per context (`src/compiler/server/nodeGraph.ml`), recorded
at the end of typing for the modules typed by the compilation: the fingerprint
of every node they produce (header and members of their types, signature and
inline body of their fields, phase 4) and, for every computation (phase 5),
what it read with the fingerprint it saw: declarations of its file (the
declaration, the imports, the module's types, the list of fields and declared
signatures for members, the position when line numbers are used), lookups
(the file the module path resolves to), nodes of other modules, `@:generic`
host modules, and the module of the macros that ran. `check_module` validates
a module of the cache whose file did not change through the graph instead of
its module dependencies: a node read is valid when its fingerprint is the
one seen and the computation producing it is valid in turn (its inputs did
not change, so it would compute the same value: what phase 5 checks), down to
the declarations of changed files, which are parsed again (the parse is then
reused to type them). Module dependencies are still checked for what the
graph does not cover (other contexts, macro modules, files declared by
macros). A module that is valid while a module it depends on is not is
reused, and the other one is typed again after it; grafting keeps the
references between them valid. A computation that ran a macro depends on
every declaration of its file, on its own module (the files its macros
declared) and on the macro's module in the macro context; the macro API
records what it reads of the types it gives to macros (header, field
signatures, enum constructors, typedef targets). Cycles in the graph are
handled like module cycles (valid results under assumption are forgotten
when the module turns out invalid).

**Status: item 2.4, first step (bodies kept when a module is typed again).**
A module is still typed again as a whole, but the body of a field is not: when
its computation is still valid, the expression the previous compilation
typed and post-processed is given back (`DepRecord.reuse_body`, set by the
server; grafting keeps the old expression of each field it reuses,
`Graft.old_body`), marked post-processed so that the filters skip it, and
the module depends again on the modules the body read (taken from its
edges). Its node keeps what it read in the graph. The validity is strict:
what the computation read did not change, the nodes it read are produced by
valid computations or by modules this compilation found valid, without the
assumptions of early cutoff (made by the validation of modules, they are
only checked after typing, and an assumption made for a body would compile
again for nothing when it fails). Not kept: fields whose type is inferred
from their body, inline and `@:generic` fields and the operators and casts
of abstracts (their body is read by others), overloads, and bodies that ran
a macro or made instances of a `@:generic`.
Two fixes on the way: the fingerprints no longer hash the markers that the
typer and the analyzer set on local variables (`VUsedByTyper`, `VAnalyzed`),
and an assumption about a node that no module produces (no fingerprint
before or after) holds.
A body is kept only when its positions are still right: the field did not
move, and the positions in other files point into files that were not lexed
again. A kept body still runs the override check of its field (it marks the
implementation of an abstract method `override`, which the HL virtual tables
read). Warm and cold outputs are compared by random edits of Heaps (HL, with
debug information) and hide (JS, with source maps); they found two older
holes, fixed too: the constructor of a `@:structInit` class is an inline
body (a structure declaring an instance copies the default values of its
arguments, with their positions), and the fingerprint of an inline body
hashes its positions relative to the field (positions that change within it,
for example its line endings, move in its consumers). The HL generator
caches structures by fingerprint, then by a structural equality that holds
the closures of lazy types equal (`compare` failed on the structures of kept
modules meeting those typed again).
Heaps: a field added to `h3d.scene.Object` 0.70 s instead of 0.89 s, its
signature changed 0.77 s instead of 0.97 s, `hxsl.Macros` moved 1.13 s
instead of 1.38 s; hide, a field added to `hrt.prefab.Object3D`: 2.6–2.7 s
instead of 2.7–3.1 s (most bodies of the prefabs read signatures of
`Object3D`, whose members are computed again: without assumptions they are
typed again).

The validation visits every computation once per compilation (pull). Its cost
per edge is kept low: the nodes known to have changed are only looked up when
there are some, and a lookup keeps the file it found until a directory of the
class paths, or the class paths themselves, changed (the graph's directory
epoch, from the changed directories the server computes anyway). hide, a
body edit: 0.09 s for the 1629 modules traced, instead of 0.2–0.28 s. A
push-based validation (from the changed declarations to their readers) would
make it proportional to the change.

The signature a field declares is a computation of its own (`declared sig`,
in `init_field`), which reads the declaration without its body; finding a
field by name reads its signature, not finding it reads that the class has
no field of that name (`DAbsent`, the `Absent` edge: valid while the members
are, or assumed and checked after typing; a constructor inherited from the
parent counts as absent), an instance of a class in a type
(`TAccess.build_header`) or the class used as a value (`Class<T>`) reads its
header, and creating the types of a module from its
declarations reads for no computation (`DepRecord.untracked`). Reading all
the members (iterating the fields, a structural unification with the
statics) reads the members node.

**Early cutoff** (item 3): a node read whose computation is invalid (its
module is typed again) but whose fingerprint is the one seen is assumed to
keep it, except the members of a type whose list of field names changed
(they change). After typing, the fingerprints computed for the typed modules
are compared with the assumptions before anything is stored: if one does not
hold, the graph stays the previous compilation's (consistent with the cache,
which is only updated at the end), the nodes that changed are remembered
with their new fingerprint (`NodeGraph.known_changed`: an edge to them is
checked against it, they are not assumed again) and the compilation is done
again with assumptions about the other nodes (`NodeGraph.Retry`, caught by
`HighLevel.entry`, at most 3 attempts; console output of macros can then
appear twice). If typing reported an error while assumptions were made, the
next attempt makes none. A function whose return type is inferred can
therefore change its body without typing its readers again, as long as the
type stays the same.

Measured on hide (a field added to `hrt.prefab.Object3D`, the base class of
the prefabs, whose build macros serialize the fields): 498 modules typed
again in 6.7 s before (one false assumption, then a compilation without
any), 67 modules in 3.3–3.8 s now, where filters and generation take
half of the time (phases 8 and 9); Heaps, a field added to
`h3d.scene.Object`: 94 modules in 1.4 s before, about 30 in 0.75 s. The dependency
keys of a constructor reached through a copy (inherited, with the type
parameters applied) were those of a member named `new`, a node never
produced, whose assumption always failed.

Work that a computation schedules (interface checks, field connections,
constraint checks) runs as part of it (`DepRecord.capture` in
`Typecore.schedule`), and reading all the fields of a class reads their
signatures. Recording and the graph are only active in the compilation
server.

Consequences: a change in the body of a function whose signature is declared
types again its module only; lines added in a file type again its module only;
a field added or a signature changed types again the modules that read the
members or that signature. Measured on Heaps (1534 modules, JS, 4 domains):

| Change | Modules typed again | Warm compilation |
|---|---|---|
| none | 0 | 0.42 s (was 0.42 s) |
| body of `h3d.scene.Object.getMaterialByName` | 1 (was 378) | 0.44 s (was 2.88 s) |
| lines added at the top of `Object.hx` | 1 (was 378) | 0.52 s (was 2.88 s) |
| field added to `Object` | 94 (subclasses, readers of members) | 1.51 s |
| declared return type changed | 94 | 1.55 s |
| lines added in `hxsl.Printer` (used by the shader macros) | 166 | 1.77 s |
| lines added in `hxsl.Macros` | 109 | 1.39 s |

Nodes are records updated in place when their module is typed again, and
edges point to them: validating all of Heaps takes about 0.1 s.

Cost of the first compilation in the server (Heaps, 1231 modules, 52 000
computations, 340 000 dependencies, 530 000 edges): 6.0–6.5 s, against
5.3 s without recording (`HAXE_RECORD=off`). Dependencies are recorded as
the entity read (`DepRecord.dep`), keys are built when the graph is stored;
the fingerprints of the nodes are computed in parallel after typing; the
edges are built once the server answered (a server task, before the
maintenance tasks) or before the next compilation uses the graph, from
lookups resolved at the end of typing (in the compilation's directory).

Every warm output is identical to a cold compilation (generated JS and
messages), and so is the whole `tests/incremental` suite, the replay of 20
Heaps commits, and 85 random edits of Heaps (moved lines, changed literals,
added fields, changed inferred return types, errors and their fixes), each
compared with a cold compilation; `@expect-retyped
<modules>` checks which modules of a case are typed again (`server/retyped`
gives them), `HAXE_GRAPH=off` restores the module dependency check and
`HAXE_GRAPH_TRACE=1` prints why a computation is invalid.

Not done yet: retyping a field instead of its module (item 2.4), the shift of
anchors of declarations that moved (item 2.2: a module whose file changed is
typed again), messages replayed per node (item 6), macros traced precisely
(phase 7: a change in a module used by macros types again every module that
ran them), and the cost of recording (about 25% of typing, with string keys).
A compilation with errors caches nothing, as before: typing stops before
post-processing, so the modules typed by that compilation have no restore
point (`save_class_state` is part of the filters, and marks fields as
post-processed), and grafting already wrote their content into the objects
of their previous version, which `mt_restore` brings back when that version
is reused. Keeping the modules without errors needs post-processing per node
(phase 8).

**Next: adding a method to a class used everywhere.** Measured on SpaceCraft
(server, parallel typing, `8b344d213`): adding `public function
benchFieldN() {}` to `Game` (used by about 80 files) takes 6.3-8.0 s, against
2.2 s for a body edit in a leaf file and 3.4 s for a body edit in `Game.hx`.
`--times`: typing 1.0-2.6 s (3300-5600 events), macros 0.3-0.7 s, HL
generation 1.8-2.3 s (classes generated again 0.7 s, link 0.2-0.6 s, hlopt
0.2-0.5 s), server module checks 0.8-0.9 s (40000 events, 0.3 s for a body
edit), filters 0.7-0.9 s. The graph trace (`HAXE_GRAPH_TRACE`) of the edit:

- *First pass: 38 modules typed again*, 19 of them because a computation read
  the members of `Game` as a whole (`node Game members`): adding a field
  changes that node, and the declaration changed, so it is not assumed to keep
  its fingerprint (`declaration_changed`). The others follow from what these
  modules produce.
- *42 assumptions fail after typing*, all on nodes of modules that this
  compilation did not type (`st.service.MissionManager.eventMission` sig,
  `ui.comp.ItemInfo` members and absences, `ui.win.menu.MenuWindow` absent
  `new`): a dependent read them and assumed they keep their fingerprint, but
  their module was never checked nor typed, and what their computation read
  did change (it read the members of `Game`), so the check after typing
  (`same_produced`) rightly refuses them.
- *The compilation is then done again without assumptions: 216 more modules
  typed again*, most of the cost (cascades through `ent.ShipStatus` members,
  `gfx.FxOptions` header, `ent.PersonaLocation` members... whose fingerprints
  did not change).

Leads, by expected gain:

1. *No redo without assumptions for this case.* When an assumed node belongs to
   a module that was not typed and its computation is invalid, type that module
   in the same compilation (a second round keeping the assumptions, with only
   the failed nodes known changed), instead of typing everything again without
   assumptions. Expected: about 38 + a few modules instead of 254.
2. *Read the members of a class at the granularity actually needed.* Find which
   code paths record `members` of `Game` for these 19 modules (a field access
   should record the signature of the field, or the absence of a name, not the
   whole members: candidates are unification with structures, static
   extensions and `using` lookups, `@:forward`, macros iterating the fields,
   `Std.isOfType`/casts, completion-like lookups) and record the field or the
   absence instead. Adding a method then only types again the readers of all
   the fields (macros iterating them, reflection) and of that name.
3. *Server module checks* (0.8-0.9 s here, 0.3 s for a body edit): they grow
   with the number of nodes found changed; cache the validity of nodes across
   the two rounds of lead 1.
4. HL generation follows the number of classes typed again (leads 1 and 2).

**Status: fuzzing (warm/cold, errors, memory).** What the fuzzer of phase 0
found on Heaps, fixed:

- *`@:generic` instances* are not modules of the graph: a body reading an
  instance, also through the body of an inline function it inlines, depends
  on the module of the generic (a `generic` node), so that an edit of the
  generic types it again.
- *Bodies reused for inlining.* A body reused from the previous compilation
  keeps its unoptimized typed expression (`cf_expr_unoptimized`), which the
  inlining of the next compilations reads.

- *Inline constructors.* `InlineConstructors` inlines the instance variables
  of the class and of its parents, with their initial values, where the
  constructor is inlined; only the constructor's body was a dependency, so
  adding a variable to `h3d.col.Sphere` left the callers of `new Sphere` (or
  of the inline `Bounds.toSphere`) without it. A use of an inline constructor
  now records the members of the class and of its parents and the initial
  value of each instance variable (an `inline` node, which instance variables
  now have), also for the `new` of an inlined body
  (`tests/incremental/cases/inline/inline-constructor-fields.hxtest`).
- *Errors of a failed compilation.* A computation the graph finds valid can
  fail when typed again through what it does not record: the hxsl build macro
  of a shader fails on the code of the shader, the shader loses its fields,
  and the cold compilation reports the errors of the modules reading them,
  which the server kept as valid (the assumptions on the nodes of a module
  typed again are dropped, so the retry on an error did not happen either). A
  compilation that checked modules through the graph and stops on an error
  is done again with the modules checked through their module dependencies
  only (`NodeGraph.modules_only`, as with `HAXE_GRAPH=off`), which also makes
  no assumption; not for display requests, which can send their result before
  typing is done. Only failed compilations pay for it.
- *Failed compilations kept alive* (parallel typing; ~165 MB each on Heaps,
  out of memory after about 12): the API of the macros was kept per domain in
  a domain-local key created per macro context (a domain keeps the value of
  every key ever created); the declared argument types of methods
  (`Typecore.declared_args`, by class) were lazies holding the typing context;
  and the modules parsed in advance by the domains typing in parallel
  (`TypeloadParse.preparsed`), with the context of the compilation, stayed in
  their table when the compilation stopped before loading them. After
  failed compilations of Heaps in parallel mode, the live heap of the server
  stays at ~175 MB (~770 MB with the first two fixes only, unbounded before).
  The last one was found from the global data of the program
  (`caml_globals`), with a search of the paths reaching the typers of the
  previous compilations.

**Status: wide invalidations on a Heaps application (hot reload).** Measured
on the Heaps samples compiled as one program (39 samples, with domkit and hxbit,
HL, `-D hot-reload`), each edit made through the server, then undone. With
`HAXE_GRAPH_TRACE` the validation takes several times longer (it formats every
edge it checks): times are measured without it, the modules typed again with it.

| Edit | Modules typed again (before → after) | Warm compilation (after) |
|---|---|---|
| body of a method of the application, of `h2d.Object` | 0, 3 | 0.4–1.0 s |
| local function added in `h2d.Flow.sync` | 7 → 1 | 1.2 s |
| empty line at the top of `hxsl/Ast.hx` | 148, and 105 failed assumptions in a variant (a second compilation) → 90 | 1.6–2.2 s |
| static added to `hxsl.Tools` (read by the shader macros) | 172 → 90 | 1.6 s |
| static added to `hxd.Math`, then removed | 24 → 23; the removal failed an assumption (compiled twice) → none | 1.4–2.0 s |
| field added to `h2d.Object` | 28 → 24 | 2.6 s |
| syntax error, then fixed | 2 → 0 | 1.3 s |

Fixed:

- *Nodes no computation produces.* The header and the members of a type are made
  from its declarations when its module is loaded (`DepRecord.untracked`): when
  its module was typed again, an edge to them was valid only if that module
  was, never assumed to keep its fingerprint. Lines added in the file of an
  enum typed again every reader of its constructors (`hxsl.Type`,
  `hxsl.Component` for `hxsl/Ast.hx`). They are now assumed unchanged when the
  declarations of their type did not change, positions aside
  (`NodeGraph.untracked_unchanged`), checked after typing like the others.
- *Lazy types in fingerprints.* A module typed again by the server can leave
  lazy types unforced (the type hints of enum constructors, forced by the
  modules reading them, which are not typed again), and a lazy hashes as such:
  the members of `hxsl.ExprDef` got another fingerprint than the cold one, the
  assumptions on them failed and the compilation was done again without
  assumptions. The lazies the fingerprints read are forced on one domain
  before the fingerprints are computed in parallel (`TFingerprint.force_lazies`).
- *A removed field.* A computation reading a declaration that is gone (a field
  removed) is not assumed to keep what it produces: the assumption always
  failed, and the compilation was done twice.

**Next: the compilation after an error.** A compilation that fails caches
nothing, but it leaves the modules it found invalid invalid (a module found
invalid sticks to it), and the attempt made after an error checks the modules
through their module dependencies only (`NodeGraph.modules_only`), which
finds many of them invalid and types them: undoing the error types them all
again (a field added to an abstract of Heaps, then removed: 340-400 modules,
3.4 s, where the edit itself typed 22; SpaceCraft's first compilation after an
error takes 14–17 s). The modules typed by the failed compilation must be
typed again: grafting wrote their new content into the objects of their
previous versions. Tried: keeping a shallow copy of every object grafted
(in `Graft.copy_into`) and putting it back when the compilation fails, then
forgetting what the failed compilation found invalid. The compilation after
the error then types one module, but the fuzzer finds warm outputs that
differ from cold ones after an error is undone (`hxd.App.new` generated
without the box of the local `engine` that its closure assigns: a body
reused without the `VCaptured` mark that the filters set), so something the
failed compilation changes is not in the copied objects. Not done: find what
(the state the filters keep per class, `cl_restore`, the post-processed
bodies kept by grafting, the variables of the bodies), or keep the failed
compilation from typing modules found invalid only through the module
dependencies.

**Status: the compilation after an error.** Measured on the Heaps samples
(a type error added to a body of `h2d.col.Polygon`, then removed): the attempt
with the graph typed 10 modules and failed; the attempt done again through the
module dependencies only typed 344 and failed with the same error; the
compilation removing the error then typed 345 again (everything the failed
one grafted), 5.1 s. When typing goes to its end with errors, the attempt is
now kept: the assumptions of early cutoff are checked as when the compilation
succeeds (what was found valid reads the same, so its errors are the ones of a
cold compilation), and the compilation is done again with the graph if one
failed, knowing the new fingerprints. The compilation removing the error types
16 modules, 2.0 s. When typing stops in the middle (an error raised: a type
not found in a signature), what was typed before depends on what was found
valid, and so do the errors (the readers of the class typed first report
errors the cold compilation never reaches): the compilation is still done
again through the module dependencies.

Found on the way, by the fuzzer: since the static variables of the macros are
initialized once in a base (`Macro statics: per-domain copy of the base`), the
values its initializers gave in a `sys.thread.Tls` (the event loop of
`haxe.EventLoop.__init__`) were missing on the other domains running macros,
and a macro using the event loop failed with parallel typing (Heaps,
`hxd.res.FileTree`: every compilation with more than one domain). The domains
running macros stand for the main thread of the macro context: they use its
thread-local storage (under a lock), a thread made by `sys.thread.Thread`
keeps its own. And an error while the interpreter added the types of the
macro context left the types added before it without prototype (see
`EvalPrototype.add_types`): every later compilation failed until the server
restarted.

**Next: the code of macros.** What is still typed again in the edits above
comes from the edges to the module of a macro that ran (`MacroModule`): it is
checked through its module dependencies in the macro context, so any change in
a file those reach (`hxsl/Ast.hx`, `hxd/Math.hx`) runs every build macro of
`hxsl.Macros`, `domkit.Macros`, `h2d.domkit.InitComponents`,
`hxd.fs.EmbedFileSystem` again and types again their modules (90 of the 90
modules after a line added to `hxsl/Ast.hx`). This is the `wait: macros` /
`macros run again` of SpaceCraft's 11–19 s compilations, where domkit builds
every component. The lead is item 11 of phase 7: record, per expansion, the
methods of the macro context it executes (every method goes through
`EvalJit.jit_tfunction_lazy` with its type and field; one comparison with the
epoch of the expansion per call), and replace the `MacroModule` edge by edges
to their bodies in the graph of the macro context (`... body` computations, which
record what the code read), with their absolute positions (code reified in
a macro carries the positions of the macro's file into its output). A change
in the macro context that the expansion does not execute, or that only adds
fields after the code it executes, then leaves it valid.

**Status: the code of macros.** An expansion depends on the code the macro
context executed for it, checked in the graph of the macro context, instead of
the module of the macro and everything it depends on:

- *Recording* (`EvalPrototype`, `EvalJit`, `DepRecord.exec_recorder`). Every
  function of the macro context records itself once per expansion (the epoch
  of the static isolation, unique per expansion), in the computation running
  the macro: the methods and the constructors (an inherited constructor as
  the one of its class), the initializers of the static variables it reads
  and the `__init__` of their class (which give the base it starts from: they
  are recorded on its first access to the variables of the class,
  `EvalValue.domain_values`). The classes it
  instantiates record their members (the methods an instance dispatches to, an
  override added changes them; the variables reflection lists), and so does a
  type used as a value (`Type.getClassFields(C)`, `Std.isOfType`). The
  recorders are made with the prototypes, which outlive the compilations,
  whether recording is on or not.
- *Edges* (`NodeGraph.MacroNode`). A function executed is an edge to its
  `exec` node in the graph of the macro context: the fingerprint of its typed
  code, produced by its body (or its signature when its type is inferred). A
  body of the macro context whose values hold positions of its file (reified
  expressions, through `$__mk_pos__`; `haxe.PosInfos`) also reads its position
  in the file, so that a line added above it runs the expansions again. A class
  instantiated is an edge to its members. Only what cannot be found falls back
  to the module of the macro (`MacroModule`, checked through its dependencies
  as before): the `__init__` of a class, a node the macro context did not
  store. The signature of a macro function in the code context (the macro is
  loaded, not run) reads the signature of the function in the macro context.
  In the macro context, a macro is never run (`macro-in-macro`): a call to it
  is not an expansion.
- *Validation*. The graph of the macro context is checked with the common
  context of the macros (its defines and class paths, made for the check when
  the compilation has not made it yet: `MacroContext.create_macro_com`), only
  when an edge of an expansion is checked. It is not typed again to check what
  it assumes, so nothing is assumed: a node no computation produces (a header,
  members made from the declarations) is valid when the declarations of its
  type did not change, positions aside, and a field still missing from a class
  when its file still has no declaration of that name. A module of the macro
  context is valid when its file did not change and its computations are valid
  (a `@:generic` instance, an `MFake` module, when the modules it depends on
  are); a node of a module the server invalidated (`server/invalidate`) is not.
  A module of the code context traced by the graph no longer checks the macro
  modules it depends on through their module dependencies: its expansions do.
- The reason a module is typed again because of an expansion
  (`ReadChanged`) names the function it executed that changed (`executed
  hxsl.Macros.buildFields (static)`); `HAXE_GRAPH_TRACE` says why the macro
  context found it changed.

Heaps samples (server, 4 domains), modules typed again (with `HAXE_GRAPH_TRACE`)
and time of the compilation (without it), compared with the compiler before:

| Edit | Modules | Time |
|---|---|---|
| static added to `hxsl.Tools` (`hxsl/Ast.hx`, read by the shader macros) | 90 → 0 (but the module of the file) | 1.50–1.59 s → 0.53–0.57 s |
| static added to `hxd.Math` | 23 → 1 | 1.32–1.34 s → 0.47–0.62 s |
| function added at the top of `hxsl.Macros` | 68 → 62: the shaders, whose code reifies expressions positioned in the file | 0.97–1.03 s → 0.93–0.97 s |
| no change, body edits, domkit and hxbit classes, field added to `h2d.Object` | unchanged | unchanged (±0.1 s) |

Found on the way: a computation whose macro code changed was checked after
what it reads in the graph, and a cycle back to it found it valid while it was
being checked (the shaders and `h3d.shader.SkinBase`): its module was kept, the
assumption made on it failed after typing, and the compilation was done twice.
A computation now checks first the edges that do not lead to other
computations (declarations, lookups, the macro context).

Not recorded: a class found by name (`Type.resolveClass`) and reflected on
without being instantiated or named in the code; the positions of an
exception thrown by the code of a macro and reported as an error.
`tests/incremental/cases/macros/executed-*` cover what is recorded (a function
not called, a static added, the function called, an override, positions,
static initializers, enum indexes, inherited constructors, reflection).

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

A module whose file has a new modification time is checked against the digest
of the content it was typed from (the digests of the last contents read are
kept with the time of the file): a checkout, a copy or a save without change
no longer types it again (SpaceCraft, the whole sources copied again: 25.7 s
→ 3.1 s). Not done yet: content hashes for the detection itself (a file
system with a one second modification time precision still misses two edits
within the same second), a resolution
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
     result must be recomputed calls `Context.setVolatile(?check)` (item 7).
     Otherwise its cached result is reused, which is
     the macro's responsibility, like undeclared file reads;
   - console output (`Sys.println`, `trace`, `Sys.print`) is written
     immediately, in no guaranteed order when expansions run in parallel, and
     is **not replayed** when an expansion is reused from the cache;
   - file writes: not tracked (see item 8).
4. **Static state isolation.** Macro-context static variables no longer carry
   state from one macro call to another.
   - **Every expansion starts from the base of the ordinary statics**: what
     their initializers and the initialization macros (`--macro`) left, the
     configuration these give included. Everything an expansion writes, in
     place too (a `static var cache = new Map()` is usually mutated in place),
     is undone when it ends.
   - **Nested expansions** (a `typeExpr` or `getType` triggering another build
     or expression macro) start from the base too, and the caller's state is
     restored on return (stack discipline).
   - **`@:persistent` means cache.** A `@:persistent` static survives
     expansions and compilations, as pure memoization: its content must be a
     function of what is put in it, reading it records no dependency, and the
     compiler may empty it at any time (at least when the macro code changes).
     It can be written anytime. Configuration written by initialization
     macros needs no `@:persistent`: it is the base of the ordinary statics,
     rebuilt on every compilation since they run first (a `@:persistent`
     variable they write is a cache too). Outside of macros it is an ordinary
     static, so the same code also works at run time.
   - `-D macro-state-check` empties the caches before each expansion and
     compares the fingerprint of its result with the one obtained with the
     caches: a difference shows an impure use (e.g. a registry that depends
     on the order of expansions).
   - This changes the meaning of `@:persistent`, which today means "survives
     across server compilations" with no contract: registries kept in it
     move to `TypeCache` (item 9).
5. **Type data: `haxe.macro.TypeCache<T>`.** Data attached to types by macros,
   given by the build of a type or computed on demand by a pure function of
   the type:

   ```haxe
   package haxe.macro;

   class TypeCache<T> {
   	/** `compute` is called for a type whose build did not give a value;
   	    its reads are recorded, its result kept until one of them changes. **/
   	public function new(?compute:Type->T);
   	/** The value of the type being built: only during its build, once. **/
   	public function setCurrent(value:T):Void;
   	/** Builds `t` if needed, then its value (from its build, else
   	    `compute`); records a dependency on that value. **/
   	public function get(t:Type):T;
   	/** Same as `get` without `compute`: null if the build gave no value. **/
   	public function tryGet(t:Type):Null<T>;
   	/** `get` by type path ("pack.Name"). **/
   	public function getByPath(path:String):T;
   	/** Every type with a value, sorted by path: only in `program()` stages
   	    (`onAfterTyping`, `onGenerate`), once every build ran. **/
   	public function all():Array<{type:Type, value:T}>;
   }
   ```

   - **Identity**: a `TypeCache` is declared as a static field, which is its
     identity: resetting statics recreates the object, not its content,
     which the compiler keeps in its graph.
   - **One producer per type**: the build of the type (`setCurrent`) or
     `compute`, never both; `compute` is not called for a type whose build
     gave a value.
   - **Values are frozen deeply** when stored: mutating a value obtained by
     `get` is an error, so that no data depends on the order of typing.
   - **Incremental**: a value given by a build is valid while that build is;
     a computed value while what `compute` read is unchanged; a reader
     depends on the fingerprint of the value only.
   - **Errors**: `setCurrent` outside of the build of a type or twice, `get`
     of a type without value when there is no `compute`, `all()` during
     typing.
   - `-D macro-state-check` also computes the values again and compares their
     fingerprints.
   - **Registries for generation** (indices of classes, program-wide ids):
     each build gives its entry, and `all()` reads them in an `onAfterTyping`
     callback, which can `defineType` the registry. Such a callback runs on
     every compilation, with early cutoff on what it produces.
   - **`Context.makeUniqueName(base)`**: deterministic unique names, local to
     the expansion key, instead of static counters.
   - Whole-program APIs (`Context.getAllModuleTypes()`, iteration over loaded
     modules) have the same constraint as `all()`: their result depends on
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
       single owner, typically the `compute` of a `TypeCache` keyed by the
       type the helper is made for, whose value is then the owner;
     - code referencing a macro-defined type by name records a dependency on
       the owning expansion (`DMacro owner`). A lookup that happens before
       the owner ran is order-dependent: it is reported by verification mode,
       and such references must go through the owner (or a `TypeCache`
       value) to be deterministic.
   - **`Context.onTypeNotFound`** callbacks can only be registered during
     initialization macros (`--macro`), in a fixed order; registering one
     during an expansion is an error. Two callbacks answering for the same
     path is an error. The answer for a path is a query keyed by that path,
     so it is computed once and cached like any other node.
7. **Volatile macros**: `Context.setVolatile(?check:Void->Bool)`. An
   expansion that called it without a check is re-executed on every
   compilation; with a check, the server calls it before the next
   compilation and re-executes the expansion only if it returns `true` (or
   throws). The check runs before typing: system APIs only (files,
   environment, time), not the other `Context` functions. If the output (and
   effect log) of the re-executed expansion has the same fingerprint,
   downstream stays green.
8. **File writes are the macro's responsibility.** The compiler does not
   track, replay or verify files written by macros, and writing a file does
   not make an expansion volatile. A macro that writes files (heaps resource
   conversion in `hxd/fs/Convert.hx` and `hxd/fs/FileConverter.hx`, hide test
   generation, hide `ModelLibrary`) manages its own outputs and caches, as it
   does today. Files it reads are dependencies only when declared with
   `registerModuleDependency` (item 3).
9. **Corpus migration** (validates the declarations on real cases):
   - **domkit**: `COMPONENTS` (and `COMPONENTS_REMAP_REV`) become a
     `TypeCache<MetaComponent>` filled by the build of each component;
     `loadComponent` becomes `getByPath`; `preload` and the `// force build`
     idiom disappear. `componentsSearchPath` and `defaultParserPath` stay
     `@:persistent` configuration, which requires
     `h2d.domkit.InitComponents.init` to run as an initialization macro
     (`--macro`) rather than as a `@:build` on `h2d.domkit.Object`.
   - **hxbit**: `SERIALIZABLES`/`UNSERIALIZABLES` become a `TypeCache` (or
     header information: interfaces, declared metadata, `@:native`);
     `markAsSerializable`/`markAsUnserializable` become initialization-time
     configuration. `IGNORED_META`, `VISIBILITY_VALUES`, `CUSTOM_GETTERS` and
     `wrapRPC` stay `@:persistent` configuration. The struct `isProxy`
     information becomes a `TypeCache`. The `PartialResolution` / `TLazy`
     workaround goes away.
   - **heaps**: `hxsl` keeps the parsed shader in a `TypeCache` (given by the
     shader's build, computed from `:src` for a shader built elsewhere)
     instead of re-parsing `:src` on every `@:import`;
     `hxd/res/DynamicText.hx` `BUILD_CACHE` and `hxd/fs/Convert.hx`
     `converts` stay `@:persistent` caches, checked by `-D macro-state-check`.
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

**Status: static isolation and `TypeCache` (first step).**
- Isolation (item 4), always on (the temporary `-D macro-isolation` is
  gone once the corpus passed with it): every expansion (`EvalMain.call_path` in the macro
  interpreter, and every callback of a macro such as `onAfterTyping`) gets an
  epoch of its own and starts from the **base** of the static variables
  (`pfields`): what their initializers left, then the initialization macros,
  which write it directly (`EvalValue.base_mode`, nested expansions
  included), so the configuration they give is kept without `@:persistent`.
  - The base changes no more once the initialization macros are done
    (`set_init_phase_done`), but in the slots of the prototypes loaded later,
    whose initializers run as an expansion (their changes to other
    prototypes, a registry, are undone) writing their own slots
    (`pinitializing`, `initialize_statics`); the reset of a new compilation of
    the server writes it again.
  - The base is **sealed** (`seal_statics`) when it may have changed: its
    arrays, objects and instances get an identity in their mark (from
    `base_mark`, the frozen groups of `@:persistent` state below it). Fully at
    the end of the initialization macros and after a reset (the copies are
    made again: `base_generation`), the new prototypes once initialized.
  - Each domain running macros reads and writes a **deep copy** of the base
    (`domain_values`), made on the first access of the compilation to a
    prototype, by identity (`slot_memos`, by domain slot: what two variables
    share stays shared); the objects of the copy are marked with the domain
    slot (negative marks). What the expansions share is not copied (frozen
    `@:persistent` groups, functions, types, mutexes).
  - The changes an expansion makes to the copy are **journaled**: the first
    write of an expansion to the slots of a prototype, or to an object of the
    copy (through the write barriers of the arrays, field writes, `Reflect`
    and the maps, which thaw frozen groups too), saves its state; when the
    expansion ends, the journal undoes them, the last first. A nested
    expansion starts from the base: the journal entries of its caller swap
    their state while it runs, and back when it ends. A speculating macro run
    again (`Claims.Invalidated`) starts from the base too.
  - What writes cannot be journaled for (bytes, buffers, vectors, atomics) is
    copied again from the base by every expansion that reads its prototype
    (`pvolatile`). The state of a regular expression and what a function
    captured are not journaled.
  - Objects of a copy that escape to shared state are copied (`export`): when
    written to a `@:persistent` variable, or to the base (an initializer of a
    prototype loaded later). One reached in place (pushed into a
    `@:persistent` array) is not: it cannot be frozen, so reading it ends a
    speculation, but the domain owning it still changes it.
  Cost: a copy of the statics a domain reads, once per compilation, then what
  the expansions write; reading a static is not slower. `@:persistent`
  statics are not copied, and keep the writes of a nested expansion (a build
  macro of an interface run from the build of its implementer may give the
  configuration, as in domkit). The interpreted program (`--interp`) is not
  affected (`cases/macros/init-macro-static-base.hxtest`,
  `cases/macros/macro-static-journal.hxtest`).
- `haxe.macro.TypeCache<T>` (item 5): identity from the static variable whose
  initializer creates it; values by type path with the module of the type
  (a module typed again must give them again), `get` builds the type and
  records a dependency on its members and its module, `compute` is
  memoized for the compilation and its dependencies are recorded again for
  each reader, `all()` only once typing is done (`onAfterTyping`).
- Whole-program APIs: `Context.getAllModuleTypes()` is an error until typing
  is done (like `TypeCache.all()`), and `Context.onTypeNotFound` can only be
  called from an initialization macro.
- `Context.setVolatile(?check)` gives the current module the `VolatileMacro`
  check policy and registers the check for that version of the module
  (`Common.volatile_checks`): the server types the module again when an
  expansion gave no check or a check returns `true` (`ReadChanged "volatile
  macro"`), and the early cutoff of the graph keeps its dependents when the
  result is the same (`cases/haxe6/set-volatile.hxtest`,
  `cases/macros/volatile-check.hxtest`).
- Tests: the `misc` projects that kept state between expansions use
  `@:persistent` (Issue11373, Issue3864, `resolution/modulestatics`) or a name
  derived from the build fields (Issue11582); the `haxe6` cases
  `all-module-types-during-typing`, `on-type-not-found-outside-init`,
  `set-volatile` pass, and `persistent-frozen` became
  `persistent-cache-writable` (a cache can be written by any macro).
- `-D macro-state-check` (`WMacroStateCheck` warnings): the signature of a
  `TypeCache` value is taken when the build gives it and compared once
  typing is done (a reader that changed a shared value), and a `compute`
  function is called a second time and must give the same value. Heaps, hide
  and the domkit sample pass it. The values are not frozen (an error at the
  write would need a frozen flag on every mutable value of the interpreter),
  and emptying the `@:persistent` caches before each expansion is not done:
  a cache filled by one expansion and read by the next one is legitimate
  (configuration), and the order-independence of the program is checked by
  `-D typer-shuffle` (phase 3.4).
- `tests/unit`: the tests that kept state between expansions in ordinary
  statics use `@:persistent` (or `Context.getType` for a type defined once);
  `cases/macros/type-cache.hxtest`; `persistent-registry` and
  `macro-static-counter` pass.
- Heaps and hide give the same output with isolation: their registries are
  `@:persistent` already, which becomes a cache.
- Corpus migration (item 9):
  - domkit: `COMPONENTS` is a `TypeCache` (given by the build of each
    component class, `all()` for `checkCSS` once typing is done), with a class
    of the same API for Haxe 5; the preload of the base components uses
    `Context.buildType` (reading a `ClassType` no longer builds the class);
  - hxbit: the configuration written by initialization macros
    (`IGNORED_META`, `VISIBILITY_VALUES`, `CUSTOM_GETTERS`) is `@:persistent`;
    the classes registered by `buildNetworkSerializable` were redundant with
    the `Serializable` interface, so `SERIALIZABLES` only keeps
    `markAsSerializable`.

**Status: items 1, 2, 6 and 11 abandoned (expansion keys, tracing, recorded
and replayed effects, macro code dependencies).** Measured on hide with the
server, the macros cost about 0.03 s of a body edit, also of an edit of a
prefab whose build runs macros: caching expansions would save almost
nothing, and the cold compilation not at all, for a large amount of tracing
and replay code. What isolation, `TypeCache`, `setVolatile` and the
whole-program rules give stays: the expansions are independent, which lets
them run in parallel (§4b). A computation that ran a macro keeps depending on
its whole module and on the module of the macro (phase 6), so it is typed
again with its module. The incremental cases that need replayed effects stay
known failures: metadata added to another type by a macro stays on the
cached type (`compiler-add-metadata-other-type`), and the types generated by
`onTypeNotFound` are not generated again when the initialization macro
changes (`on-type-not-found`).

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

**Status: DCE summaries (first step of item 3).** In the compilation server,
the marking that the expression of a field does (`Dce.expr`) is computed as
a list of effects (`Dce.summarize`, which mirrors `expr`): the types it marks
already flattened (what `mark_t` reaches is a function of the type), each
effect once. The summaries are kept by field with their expression, and a
DCE run applies them while the expression is the same object, which it is
for a module that is not typed again. Reachability is still computed by
every run, from the summaries (no expression is walked). hide, a body edit:
the marking takes 0.06 s instead of 0.14 s; the output is identical
(`tests/unit` on JS and HL, Heaps, hide). Outside of the server, DCE walks
the expressions as before.

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

**Status: fragments per type (first step, before phase 8).** The types are
generated apart (in parallel, `Genjs.generate_types_parallel`, now with
source maps too), each in a fragment: its code, the packages it creates
(at their offset), its statics and `__init__`, the features it read (with
their value) and added, and its source map, encoded from an empty state with
its own sources. The server keeps the fragments
(`Genjs.fragments`), and a fragment is reused while what generating it read
is the same: the type, its kind, super class and interfaces (the same
objects), the names, kinds, flags (but the markers of the filters and DCE) and
metadata of its fields, and their types and post-processed expressions (the
same objects, or the same fingerprint: some filters build them again on
every compilation, such as the field initializations of the constructor),
plus the defines, the configuration of the generator and the features read.
This works because a module that is not typed again keeps its objects
(the in-memory cache restores its post-processed state); without phase 8, a
filter that changes an expression of a cached module produces a new
fingerprint, and the fragment is generated again.
- Linking (item 3): fragments are put in the order of the types; the
  packages, then the code; the source map of a fragment is transcoded (the
  indices of its sources become those of the output, which a copy keeps when
  they are only shifted) and its first mapping encoded relatively to the
  previous one. When the state does not allow it (a current expression, the
  first mapping at the previous position, a package created inside the
  type), the type is generated again directly in the output.
- `run_expression_filters_safe` keeps the same option of `cf_expr` when a
  filter returns the same expression (the expression filters run again on
  every type with `-debug`, and every field looked changed).
- Output identical to the sequential generation, source maps included:
  `tests/unit` on JS (default, ES6, `-debug`, classic, unflatten, source
  maps), hide with `-debug`.
- hide, a body edit in a view: generation 0.85 s before, 0.5–0.65 s now
  (merging the source maps is most of what is left); Heaps (no source map):
  0.18 s before, 0.09–0.14 s.
- The transcoded mappings of a fragment (when the indices of its sources
  are not only shifted) are kept with the indices they were encoded for, and
  reused while they are the same: hide, merging the source maps takes 0.045 s
  instead of 0.18 s, the generation 0.2–0.3 s.

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

**Status: item 3, first step (optimization memo).** The cache that `Hlopt`
already had for this (keyed by a signature never computed, so disabled) is
active: the key is a digest of what `_optimize` reads of the function, that
is its code with the indexes of constants, functions, globals and types
replaced by zero, the number of arguments, and for each register the part of
its type the optimizer inspects (object or struct with the packed flags of
its fields, super classes included, function with a dynamic return,
abstract, `Dynamic`). The optimizer only rewrites registers in place and
replaces instructions by `nop`s, never moves one, so a hit patches the
current indexes back into a copy of the cached code at the same positions and
replays the remapping (`remap_fun`) on the current function: the registers,
debug information and assigns come from the current compilation, never from
a previous one (types of a previous compilation are structurally equal but
cyclic, and the writer looks them up by structural comparison). An entry not
used by the last three generations is dropped, as before. On Heaps
(`-D apicheck`, 4 domains), a body edit takes the `opt` step from 1.05 s to
about 0.45 s, the rest being the key (a `Marshal` and a digest per function,
about 0.1 s) and the remapping; a cold build pays the key, about 0.1 s. The
bytecode is identical to the cold one and to the previous compiler. Without
the forced minor collections (section 4b, item 8), the step takes 0.12 s.
The next step is item 1, so that the code generation itself (`generate;hl;
functions` in `--times`, 0.9 s of the 1.3 s left) is per function.

**Status: item 1, first step (generation memo, server only; replaced by the
linker, see the last status below).** Instead of
symbolic operands, the generation of each class is recorded and replayed
(`genhl.ml`, "generation memo"):

- *Recording.* While a class is generated, every call that reads or changes
  the tables of the context (HL types, constants, functions, globals,
  natives, method wrappers, the closures of enum constructors, the names of
  local functions, warnings) is an event holding typed AST values and HL
  values; the functions it emits are kept with the positions of their
  instructions. A call made inside an event is part of it, and the same type
  or key again in the same class is not recorded (it only reads what the
  first call created).
- *Validity.* The memo is used when the declarations that the generator reads
  are the same as in the previous compilation (for every type: path, flags,
  kind, super class and interfaces, metadata without positions, and for each
  field its name, kind, flags, metadata, type and whether it has an
  expression; for enums, typedefs and abstracts their constructors, target or
  underlying type, casts and operators), with the same defines and resources;
  then a class is replayed when its expressions are the same: its module was
  not typed again (the filters build the expressions again in every
  compilation, from the same typed AST), or they have the same fingerprint
  (positions included; the flags of the local variables that the generator
  does not read are masked, since the analyzer marks the variables, which
  the compilations share).
- *Replay.* The events are replayed in order in the new context, so that the
  indexes are allocated exactly as by a full generation; the functions are
  remapped: constants by value, functions and globals by key or by the
  results of the replayed events (local functions, wrappers, constant
  strings), HL types by their key in the caches of the context (path,
  structure, tuple), the debug information built again by `make_debug`
  (lines kept when the lexer did not read their file again). A failure of the
  remapping generates everything again without the memo.
- *Checks.* `-D hl-memo-check` compares every warm generation with a
  generation without the memo (dumps on a difference), `-D hl-memo-trace`
  says why the memo is not used. A fuzzing of random edits of Heaps on HL with
  the check found no difference, and the bytecode is identical to the
  previous compiler.
- Found on the way: `RenameVars` added a `:realPath` to a variable every
  time it renamed it, so the metadata of the variables grew with each
  compilation of the server; the previous name now replaces it.

Heaps (`-D apicheck`), a body edit of `h3d.scene.Object`: 1119 of the 1121
classes are replayed, `generate;hl;functions` takes 0.4–0.6 s instead of
0.8–0.9 s, the compilation 1.1–1.35 s instead of 1.5–1.7 s. A compilation
where the declarations changed (a field added) generates everything and
records it, about 0.35 s more than without the memo. The rest of the replay
is the replayed calls and the remapping (half each); symbolic operands
(item 1 proper) would remove the remapping, and the post-processed copy of
phase 8 the fingerprints of the modules typed again.

**Status: generation memo on SpaceCraft, and the next step.** On SpaceCraft
(4300 classes, server, body edit in a leaf file: 4.4 s), the HL generation
takes 3.3 s, of which 2.1 s replay the memo into a new context: converting
the types again (`to_type`, 0.9 s, half of it the fingerprints of anonymous
structures), remapping the emitted functions (0.6 s), the class types and
globals (0.25 s). The replay exists only so that the indexes are allocated as
by a cold generation (identical output). Done meanwhile: the functions are
written in parallel (write 0.64 s -> 0.43 s), `Hlopt.optimize` copies the code
before changing it in place, so the memo shares the code it emitted instead
of copying every function. Tried and dropped: optimizing each function as soon
as it is emitted, while the generation goes on (2.6 s -> 4.3 s for the
generation: the minor collections of the domains optimizing stop the domain
generating, which is the critical path); generating the classes on several
domains, each in a context of its own, then replaying the recordings in order
(the replay costs about as much as the generation).

**Next: no replay (items 1, 2 and 4), and a hot reload delta.** The plan below
removes the replay and prepares a faster hot reload in the HashLink VM; it is
to be done in this order, steps 1 to 3 in the compiler, step 4 in HashLink
(branch `haxe6-annotations`).

1. *Symbolic generation per class.* The generation of a class produces an
   object: its functions with symbolic operands (constant strings, ints,
   floats and bytes by value; functions by `(class path, field)` or, for local
   functions, wrappers and enum closures, by a key derived from their owner
   and their position in it; globals by name; HL types by
   `hl_type_key`), its HL types (protos of its classes, enums, virtuals of the
   structures it uses, by key), its natives and constants. Nothing in it
   depends on the other classes or on the order of the generation. The server
   keeps the objects of the classes whose expressions did not change (the
   validity rules of the memo above) and generates the others again.
2. *Linker.* One pass over the objects in the order of the types: the tables
   (types, globals, strings, ints, floats, bytes, natives, functions,
   constants) are numbered in a canonical order (first use while walking the
   functions in order, or sorted by key: to decide by measuring), then the
   operands are resolved, in parallel by function, and the bytecode written
   (already in parallel). Cold and warm builds both link: the warm output is
   the cold one. Expected on SpaceCraft: the generation of a leaf edit from
   3.3 s to about 0.8 s (link and write), the cold build slightly faster (no
   memo recording). The output changes once compared to the current compiler
   (another table order), checked with `tests/unit` on HL (JIT and HL/C) and
   `-D hl-memo-check` adapted to compare warm and cold.
3. *Delta for hot reload.* Besides the full `.hl`, the server writes (with a
   define, e.g. `-D hl-hot-reload-delta`) a delta of the classes generated
   again: their functions, the types, globals, strings and natives they add,
   referencing what already exists **by the keys HashLink's hot reload uses**
   (`hl_code_hash`: functions by object name and field name, globals by
   signature, types by name and structure), so that the delta can be linked
   against the running module without its indexes. One section per class,
   each with its relocation table: a section can be read and applied alone
   (streamable).
4. *HashLink: loading a delta.* Today `hl_module_patch` (`src/module.c`)
   reloads the whole new module, hashes every function (`hl_code_hash_*`),
   matches them with the running ones by signature, JIT-compiles the ones whose
   hash changed and patches their pointers, and aliases the types whose layout
   did not change. A delta loader reuses these steps on the sections only:
   resolve the relocations against the running module (its hash tables),
   allocate the new globals and types, JIT the functions of the delta, patch.
   The full reload stays as the fallback (types whose layout changed are
   refused, as today).

Constraints for steps 1 and 2 coming from step 4: the keys of the symbols must
be the identities HashLink matches (names, not indexes); a class object must
carry enough to tell which of its types changed layout (its protos and enum
constructors), so that the delta can say so instead of the VM comparing every
type; the canonical order of step 2 must not be needed by the delta (a delta
only refers by key).

**Status: steps 1 and 2 done (objects and linker, `genhl.ml`, "objects and
linker").** The replay of the generation memo (events, mapper) is removed.

- *Objects.* The generation of a class produces an object: its functions,
  optimized, the natives it declares and the warnings it emitted. The
  operands are the symbols of the generation context: the indexes of its
  tables of strings, ints, floats and bytes (by value), of functions (by
  `(name, path)`), of globals (by name, or by value for constants) and of
  debug files (by name). A table only grows and keeps the key of every index
  for the lifetime of the context, which the server keeps with the objects;
  the HL types are the types of the context, built once, holding symbols as
  well. The linker never reads the order of the symbols, so a symbol is a key.
  What depended on the other classes or on the order of the generation is
  named after its owner: local functions are `#k` and method wrappers `#wk`
  of their class (a class generates its own wrappers; hl2c numbers the
  wrappers of the same type), an enum constructor used as a value is the
  function of the constructor, and the functions of the enum constructors are
  generated again at every link from the current enums (their positions are
  not part of the declarations compared). An object records the names of the
  HL types of its class (instances and statics) for the delta of step 3.
- *What is kept.* The context and the objects are kept when the declarations,
  defines and resources are the same (the rules of the memo); a class is
  generated again when its body changed (same rules) or when the lexer read
  one of its debug files again (its lines may have moved). The entry point is
  generated at every compilation. Found on the way: the declarations compared
  did not see a changed constraint of a type parameter (retyping updates the
  parameter in place, and its previous fingerprint was computed lazily from
  it, `inference/generic-constraint` on HL); the constraints are captured.
- *Linker* (cold and warm builds). The symbols of the objects (each in the
  order of its first use) are listed once per object, in parallel; one
  sequential pass walks the objects in the order of the types (the entry
  point last) and numbers every table by first use, then what the symbols
  use (types, globals and their types, constants, natives, the enum
  constructors called); `""` is string 0. Functions are numbered by their
  position (objects in order, then the enum constructors used), natives after,
  sorted by library. The types are copied with the final indexes, the
  functions remapped in parallel, then the existing writer runs. First use was
  kept over sorting by key: the walk is linear, and the cost is in the tables
  of types, whose lookups are by shape (the same constructors with the same
  class, virtual and enum records): hashing types structurally and comparing
  them physically put the many equal boxes (`HNull i32`...) in one bucket
  (walk and types 0.1 s -> 0.03 s). The writer also finds a type by its shape
  before the structural map (write 0.15 s -> 0.09 s).
- *Checks.* `-D hl-memo-check` compares the bytes written by the warm
  generation with those of a generation from scratch in the same compilation
  (dumps `.memo.txt`/`.cold.txt` on a difference). The output is identical with
  `-D typer-shuffle` and any `-D parallel-typing`; two fuzzing runs of Heaps on
  HL (seeds 1 and 2, 86 compilations) found no difference; `tests/unit` passes
  on HL (JIT and HL/C, as before), `tests/incremental --add-target hl` fails
  only the 4 cases that expect `out/main.js` (5 before, see above), the server
  tests as before. The output changed once compared to the previous compiler
  (table order, a debug file for positions without file).

Heaps (`-D apicheck`, server, a body edit of `h3d.scene.Object`, 3 of 1126
classes generated again): the HL generation takes 0.22–0.40 s instead of
0.79–0.88 s (the classes 0.04–0.1 s, declarations 0.03–0.04 s, entry point
and enum constructors 0.03 s, link 0.05–0.07 s, write 0.1–0.2 s), the
compilation 0.73–0.94 s instead of 1.33–1.47 s. Cold (CLI) the generation
takes the same time (1.26–1.30 s against 1.26–1.31 s): the link (0.06–0.08 s)
is paid by the faster writer. Not measured on SpaceCraft (not available
here); scaling the fixed costs of Heaps by the number of classes gives about
1 s for its leaf edit, of which the writer and the link are most: the next
steps there are keeping the entry point and the enum constructors, and a
link that starts again from the first object that changed.

**Status: structures and writer.** The structures were cached by
fingerprint, then by an equality of their OCaml values (positions and
metadata included): a structure written at several places did not match the
others of its bucket, each new one was compared to all of them, deeply.
`tests/unit` on HL spent 10 s in `generate;hl;functions` for that. The
equality is now on what `to_type` reads (field names, kinds and types,
declarations by path, type parameters and `hl.Abstract` names physically, as
their constraints and names give the type), with cycles assumed equal: the
generation of `tests/unit` takes 0.55 s, and structures equal but for their
positions are one virtual type (fewer `tovirtual` between equal virtuals; the
output changed again). The writer gathers the types of each function on the
domains of the pool, then merges them in order, and merges the structurally
equal types with a hash table instead of a map (its types 0.044 s -> 0.03 s on
Heaps; writing the functions is already parallel). Reusing the bytes of the
functions of a previous write was not done: after an edit that adds a
constant, the numbers by first use move for every function that comes after,
so few functions keep their bytes. Then: the writer lists the types once per
part of 128 functions, in parallel, before the merge; the hash of the shapes
reads only the first arguments and fields (the large virtuals were hashed
whole at every lookup); the entry point finds the classes implementing an
interface in a table built once (it walked all the types for each
interface). Heaps, median of 12 leaf edits in the server: 0.25 s of HL
generation (write 0.09 s, link 0.075 s, declarations 0.035 s, entry point
0.025 s, the classes generated again the rest); a cold build 1.15–1.23 s
(1.23–1.36 s before this work).

**Status: classes generated in parallel.** The objects being independent of
each other, the classes to generate (all of them cold, the ones whose body
changed in the server) are generated on the domains of the pool, one task per
class, the classes with the most fields first; then the entry point, the enum
constructors and the link, as before. What the generation shares:

- *Tables of symbols.* Each table has a lock, and each domain a cache of the
  keys it already met (reset when the context changes); the ids only name
  keys, so the order in which the domains allocate them does not matter.
- *HL types.* They are built under one lock (reentrant on its domain), as
  before; a type is published, in lock-free tables (`Saturn.Htbl`) read by the
  other domains, when the outermost construction that created it ends: no
  domain sees a class type whose fields are not computed yet. The tuples are
  built under the lock.
- *Per domain.* The state of the function and of the class being generated
  (`m`, `cur`, `rec_cache`, `ct_delayed`) is in a copy of the context per
  task; the lines of the debug information are read from a table of the lines
  of each file per domain (`Lexer.find_line` keeps a search hint in the file,
  which the domains would share).
- *Warnings and errors.* The warnings are kept by the objects and emitted in
  the order of the types after the generation; when a class fails, the
  generation is done again sequentially, from a new context, which reports the
  error of the first class in the order of the types.

`-D hl-gen-sequential` generates the classes on one domain: the output is the
same, byte for byte (Heaps, `tests/unit`). Found on the way, by the warm/cold
check: the cast of an object to an interface looks for the field caching the
interface in a map whose keys are compared structurally, and a key could be
added while its virtual type was still being built (its fields come later),
so the map was out of order and the cast found the field or not depending on
the order of the construction; the keys are compared one by one.

Heaps (`-D apicheck`, cold, 4 cores): `generate;hl;functions` takes
0.35–0.44 s instead of 0.85–0.96 s (`-D hl-gen-sequential`); the HL generation
1.38–1.55 s instead of 1.94–2.02 s (this machine is slower than for the
measurements above). A leaf edit in the server generates 3 classes, below the
16 classes from which the generation goes parallel.

**Status: metadata added again by macros.** The objects are kept only when
the declarations are the same, metadata included: domkit adds `@:uiComp` to
the class of a component declaration (`FlowComp implements
ComponentDecl<h2d.Flow>` gives it to `h2d.Flow`) each time its build macro
runs, so a class kept in the cache got the entry once more at every other
compilation, and every class was generated again (the Heaps samples with
domkit: 0.7–0.9 s of HL generation for a body edit, 0.19–0.22 s now). The
metadata of the declarations is compared without its repeated entries
(`memo_same_meta`); `-D hl-memo-trace` prints the metadata that differs.
Still generated from scratch: any edit that changes the declarations (a field,
a static or a method added, a signature changed), 0.7–0.9 s on these samples
and probably most of the 2.1 s of SpaceCraft (whose fuzzing measured mostly
such edits).

**Status: declarations changed.** The context and the objects are now kept
when the declarations change; only what depends on the change is built again
(`memo_diff`, `memo_rebuild`):

- *Diff.* The items of the declarations are grouped by type: the header (path,
  flags, metadata, kind, super class, interfaces, parameters), each static,
  each field, the constructor. A class whose header changed is built again
  with all its members; otherwise the statics or fields that changed (or were
  added, removed, moved) are listed by name. An enum that changed is built
  again. A typedef or an abstract that changed, an extern class, a member with
  `@:hlNative`, a type that changed kind still generate everything from
  scratch. The overridden methods are compared as well (a method that becomes
  virtual changes the class).
- *Types built again in place.* The HL types of the classes that changed
  (instances for the fields, statics for the statics and the constructor),
  of their subclasses and sub-interfaces, and of the enums, are taken out of
  the caches and built again **in the same records** (`class_proto`,
  `virtual_proto`, `enum_proto`): the objects kept refer to these records, so
  they see the new layout without being touched, and the linker writes the
  new one. A record that cannot be reused (a class becoming a structure or an
  interface) generates everything from scratch.
- *Objects generated again.* Besides the classes whose body changed, an
  object is generated again when it calls a function whose declaration
  changed, or uses (in its registers, function types and `OType`s) a type
  whose layout changed: the index and type of its fields and virtual methods,
  its functions, interfaces, super class, and its members whose declaration
  changed (those of its super classes included). A member added does not
  change the layout (the code reading it was typed again), nor a member
  removed; a field added shifts the fields of the subclasses, whose users are
  generated again.
- The fingerprints of the bodies of the modules typed again are compared on
  the domains of the pool (0.23 s -> 0.06–0.1 s on the samples).

Heaps samples (server, `-D hl-memo-check` on all of them: the same bytes as a
generation from scratch): a static added to `hxd.Math` keeps 923 of 949
classes, 0.25–0.31 s of HL generation instead of 0.9 s; a method added to
`h2d.Object` keeps 905, 0.35 s instead of 0.7–0.9 s; a field added to
`h2d.Object` keeps 825 (its 24 subclasses and their users), 0.37 s instead of
0.7–0.9 s; a method added to a domkit component keeps 919. What remains of
these 0.25–0.37 s: the writer (0.08–0.15 s), the comparison of the
declarations (0.06 s), the link (0.05 s), the entry point (0.025 s). The
fuzzer adds methods and static methods as well (`method`, `static-method`):
three seeds on the samples (105 compilations, 26 such edits) found no
difference, but for the order of a warning when a method with a body is added
to an interface (`WExternWithExpr`, emitted earlier by the warm compilation
than by the cold one; on the typing side, the compilation fails).

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
the same scheduler. Today, typing runs on several domains (§3.5), and the
other operations are parallel in their own places (filters and analyzer per
type, DCE marking, `genjs` types, `genhl` function compilation and
optimization and writing, `hl2c` modules, `genjvm` classes, the XML output,
the fingerprints of the node graph), all on the pool of the compilation (`ServerCompilationContext`, one
domain per hardware thread, kept by the server across compilations); the pool
is only released to run `--interp` (`MacroContext.interpret`). The target is a
single model for the whole compilation.

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
   | Macro expansions | expansion | static isolation (phase 7, item 4) makes expansions independent; the eval interpreter keeps its compiled code shared and read-only, and its mutable state (static layers, call stacks, caches) per domain. Until this is done, macro expansions run on a single lane while other queries keep running (since §3.5 step 13, expansions run on several domains, speculating: what they share is ordered by the claim of the macros, the `@:persistent` state being frozen between batches with a write barrier) |
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
6. **Configuration.** Enabled by default, the pool having one domain per
   available core. `-D parallel-typing=<n>` sets how many of them type (4 by
   default, fewer on a machine with fewer cores), `0` types
   on a single domain (same results). Beyond a few domains, the macros (one
   at a time, the critical path of typing) wait for the typing lock more than
   the other domains gain. (Since §3.5 step 13, most macros run speculating on
   several domains at once, reading the frozen `@:persistent` state, and their
   calls take the typing lock only for the API functions that type or change
   the compilation; loading a macro is still done on one domain at a time.)
7. **Measurement.** The scheduler reports per compilation the busy time per
   domain, the critical path (longest chain of dependent queries) and the time
   spent waiting, per operation, in `--times` and `server/stats`.

8. **Allocation.** With OCaml 5, a minor collection stops every domain: the
   more a domain allocates, the more the others wait, so allocation limits
   how far parallel work scales. To do: a global search for avoidable
   allocations in the compiler (strings built only to be compared or hashed,
   such as `s_type_path` keys, intermediate lists and tuples in hot paths,
   closures allocated per call, `Printf.sprintf` in non-error paths, options
   wrapping values that are always present), measured with the minor
   collection counts of `--times` and the gdb sampler, starting with typing,
   filters, the analyzer and the macro interpreter. Example: the dependency
   recording of phase 6 built a string key per read; recording the entity
   instead (keys built when the graph is stored) cut its cost by a third.

   **Status: forced minor collections.** `Array.make` of an array larger
   than 256 words whose initial value is a block of the minor heap first
   empties the minor heap (the runtime does not want the new major block to
   point into it), a stop-the-world collection of every domain; `Array.init`,
   `Array.map`, `Array.of_list`, `DynArray.to_array` and `Hashtbl.copy` go
   through it with a freshly allocated first element. A gdb breakpoint on
   that line of `runtime/array.c` counts them: Heaps on HL forced 964 of them
   in a cold compilation and about 1000 per warm compilation of the server
   (the debug arrays of `make_debug` and `Hlopt.remap_fun`, the key of the
   optimization cache, the edges of the node graph), hide on JS 153 (the
   copies of `RenameVars` overlap sets, the inputs of the JS fragments).
   `LargeArray` (`src/core/ds/largeArray.ml`) creates such arrays with an
   immediate value and sets the elements after; the debug arrays start from
   a constant tuple. Two forced collections are left per compilation. Heaps
   on HL: the generation takes 1.9 s instead of 2.5 s cold (the optimizer
   0.77 s instead of 1.07 s), and a warm body edit 1.2–1.4 s instead of
   1.6–1.9 s; the output is identical.

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
- **Parallel macro execution.** Expansions run on several domains, speculating
  (§3.5, steps 10 and 13); what still serializes is the loading of a macro and
  the API functions that type or change the compilation. The interpreter
  itself got faster too (closure compiler improvements: macro execution of
  Heaps 0.57 s → 0.39 s).
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
  expansions), `@:persistent` caches emptied between expansions
  (`-D macro-state-check`), `TypeCache` (build or `compute`, errors, `all()`),
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
- Parallel compilation tests: identical output with `-D parallel-typing=0`,
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
