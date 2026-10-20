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
| Macros | Automatic dependency tracing, *volatile* macros re-executed with early cutoff on their output. **Macro statics are reset for every expansion**; `@:persistent` is reserved for initialization values (frozen afterwards); state that must outlive an expansion uses dedicated declarations (`MacroArray`, `MacroMap`, `MacroTypeData`, `MacroCache`, `MacroIndex`). File writes are the macro's responsibility (not tracked by the compiler). |
| Target release | **Haxe 6.** Work happens on the long-lived `haxe6-incremental` branch, regularly merging `development` in. New warnings (phase 1, cycles) may be backported to a Haxe 5.x release as a transition. |
| Compatibility | Breaking changes accepted for Haxe 6, with a warning phase before errors. |
| Macro context | The macro and target contexts keep **separate query graphs** (different defines, platform and standard library overrides), but **share parsing**: a parsed module is reused by the other context when the defines read during its parsing have the same values in both (phase 3.6). |
| Static initialization order | Types are output and static vars initialized in **dependency (topological) order**, ties broken by path. All `__init__` blocks run in a **first pass**, before any static var initializer, in the same order. |
| Error reporting | **Configurable.** By default every independent error is reported: a failed query is marked failed, its dependents are skipped without reporting cascading errors, and typing continues. A flag stops after the first (or N) errors, in deterministic order. |
| Storage | **In memory**, preserving the physical identity of objects. hxb is not on the critical path; it remains a persistence format and a verification tool. |
| Positions in inlined code | The inliner replaces positions with the call site's (`map_pos`, `src/optimization/inline.ml`), so they are excluded from fingerprints. Exceptions: `-D keep_inline_positions` (already part of the define signature; relative positions are then included in the body fingerprint) and self-calling closures (relative positions included). |
| Validation corpus | Repository tests (`tests/unit`, `tests/server`, `tests/display`); Heaps, hxbit and domkit, for their heavy use of macros. |
| Typing order | **Fully demand-driven.** The typer passes (`PBuildModule` … `PFinal`) and the `delay`/`flush_pass` scheduler are removed. Typing is a set of queries (`declare`, `resolve`, `header`, `members`, `sig`, `body`, `checks`) that are the same nodes as the incremental graph. |
| Typing root | `program()` types **all fields of all loaded modules**, as today: errors in unused code are still reported. On-demand changes the order, not what gets typed. |
| Cycles | **Every query cycle is an error**, detected generically by the query engine, with no partial view, placeholder or retry. Main case: build macros (requesting the member set of a type whose build macro is running, directly or through another build macro). Only exception: a field referring to itself inside its own body. |
| Enforcing queries | Entity contents become **abstract types** behind accessors, so that the OCaml compiler rejects any read that bypasses a query. Post-typing stages use a frozen view. |
| Parallelism | **Design goal.** Body queries must be safe to run in parallel (OCaml 5 domains, `src/context/parallel.ml`), with deterministic results. |

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

### Phase 1: semantic rules (Haxe 6 breaking change)

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

Consequence: type parameter constraints no longer need a late pass
(`PCheckConstraint`, `delay_if_mono`). Constraints on a field's signature are
checked when the field's typing ends, and constraints on local types when the
body's typing ends, since no monomorph can be bound later.

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
   - `cl_descendants`: recomputed by the aggregation pass (phase 8) from the
     `cl_super` and `cl_implements` of all types; no longer stored
     persistently.
   - DCE and post-processing flags (`CfUsed`, `CfPostProcessed`, …): moved to
     post-processing data (phase 8), out of `cf_flags`.
   - `m_features`: computed per module from its post-processed AST.
   - Metadata added by a macro from **another** module (`addMetadata`):
     recorded as an effect of the expansion (phase 7) and replayed.
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
| `header(T)` | type parameters, constraints, super class, interfaces, abstract underlying type, typedef target | `resolve` of each hint | first half of `PBuildClass` |
| `members(T)` | member set (names, kinds, access, declared hints) **after build macros**, properties connected to accessors, abstract `from`/`to`/operators | `header(T)`, `members` of ancestors (`@:autoBuild`), build macro expansions | rest of `PBuildClass`, `PConnectField`, `cl_build` |
| `sig(f)` | fully resolved field type, constraints checked | hints, or `body(f)` for an inferred type | `TLazy` on `cf_type` |
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
   `Header.super c`, `Members.fields c`, `Sig.field_type cf`, `Body.expr cf`…
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
  annotation on one of them. The only exception is a field referring to itself
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
- Display requests demand only the queries needed for the cursor position,
  plus `checks` and `body` of the display file for diagnostics.

#### 3.5 Parallel body typing

`body(f)` queries are independent once their signatures are available, so
`program()` can run them in parallel on the `Parallel` pool. Requirements:

- Query states and memoization are thread-safe (a `Running` query demanded
  from another domain waits; demanded from the same domain, it is a cycle).
- Shared mutable state is turned into queries or protected:
  - module loading (`declare`) and the module table;
  - `@:generic` instance creation (query keyed by generic and type arguments);
  - `a_status` of anonymous structures (closed per phase 1, so read-only after
    the defining field is done);
  - the eval macro interpreter (single domain at first: macro expansions are
    serialized);
  - counters used for naming (generated locals, anonymous types) become
    per-query and deterministic.
- Output is identical whatever the number of domains (checked by verification
  mode with `-D typer-domains=1` vs `N`).

#### 3.6 Parsing shared between the macro and target contexts

The macro context (`com.get_macros()`) and the target context keep separate
query graphs: they have different defines, a different platform, and resolve
different standard library overrides (`std/<platform>/_std`), so their headers,
members and bodies cannot be shared. Parsing, however, is the same work in both
contexts whenever conditional compilation evaluates the same way.

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
   values), typically one for macro and one for the target when the file
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

### Phase 5: dependency recording

Most dependencies are recorded inside the phase 3 accessors: `Header.*` and
`Members.*` record `DShape`, `Sig.*` records `DSig`, `Body.*` records `DInline`,
and `resolve` records `DLookup`. The table below lists where each kind
originates, which is also the checklist for the accessor migration.
Recording goes through an `Incremental.record dep` that is a no-op outside the
server:

| Dependency | Where |
|---|---|
| `DLookup` | `Typeload.load_instance`/`load_type_def`, import resolution (`ImportHandling`, `typeloadModule.ml`), `TyperDotPath`, class path lookup (including **failed** lookups) |
| `DSig` | `FieldAccess`/`Fields` (field access), `Calls`, `CallUnification`; reading `cf_type` of other modules' fields |
| `DShape` | field lookup (`Fields.type_field`, `class_field`), `using`, structural unification (`tUnification.ml`, `TAnon` against `TInst`), `OverloadResolution`, `Matcher` (enum constructors), override and interface checks (`TypeloadCheck`), `AbstractCast` (`from`/`to`) |
| `DInline` | `Inline`, `Generic`, `MacroContext` (executed code) |
| `DMacro`, `DFile` | phase 7 |

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

### Phase 6: per-field validation and retyping (server core)

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
4. **Same queries, cold or warm**: the phase 3 queries check the graph before
   computing. If a cached node can be validated green, its stored result is
   used; otherwise the query recomputes it. There is no separate "cached
   module" path anymore.
5. **Display and completion**: the displayed file is always retyped (current
   `AllowPartialTyping`), but benefits from early cutoff for everything else.
6. **Errors and warnings**: stored per node and replayed when the node is
   green, replacing the current handling of modules with errors.
7. **Initialization macros** (`--macro`) and global callbacks: always executed,
   last, with early cutoff on what they produce (phase 7).

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
   `DLookup` + `DShape`, `typeof`/`typeExpr` the dependencies of the triggered
   typing, `getBuildFields` and `getLocalClass` the context, etc.).
3. **Interpreter sys primitives** (`src/macro/eval/evalStdLib.ml`):
   - `File.getContent`/`getBytes`, `FileSystem.exists`/`stat`/`readDirectory`:
     `DFile` (content, or sorted listing for a directory);
   - `Date.now`, `Math.random`, `Sys.getEnv`, `Sys.command`, `Sys.time`,
     sockets: the expansion is marked **volatile**;
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
     as a sub-expansion recording its dependencies (files read, types
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
   - The aggregation reduction (`gather()` then `defineType` of a registry
     type in a `program()` stage) is reused from the previous design: it runs
     on every compilation, with early cutoff on its result.
6. **Recorded and replayed effects**: `defineType`, `defineModule`,
   `addMetadata`, `addResource`, `registerModuleDependency`, callbacks
   (`onGenerate`, `onAfterTyping`, `onTypeNotFound`), messages, and the writes
   to the declarations of item 5 are logged per expansion. When the expansion
   is green, the log is replayed instead of running the macro.
7. **Volatile macros**: re-executed on every compilation. If their output (and
   effect log) has the same fingerprint, downstream stays green.
8. **File writes are the macro's responsibility.** The compiler does not
   track, replay or verify files written by macros, and writing a file does
   not make an expansion volatile. A macro that writes files (heaps resource
   conversion in `hxd/fs/Convert.hx` and `hxd/fs/FileConverter.hx`, hide test
   generation, hide `ModelLibrary`) manages its own outputs and caches, as it
   does today. Files it *reads* are still `DFile` dependencies (item 3).
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
prerequisite for running macros in parallel later (they are serialized in
phase 3.5).

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
   by `PostProcessed(cf)`, depending on `FieldBody(cf)`, the `DSig`/`DShape`
   read and the platform configuration. This covers most of `run_safe_filters`
   and `destruction_on_scom`.
3. **Global passes, rewritten over per-field summaries**:
   - **Initialization order**: a graph of "static initializer or `__init__`
     of type A references type B" edges, built from summaries. Types are
     ordered topologically (ties and cycles broken by path, a cycle being
     reported as a warning when it involves static var initializers). This
     order is shared by all generators: types are output in it, all `__init__`
     blocks run first in this order, then all static var initializers in this
     order.
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

### Phase 9: JS generation

`src/generators/genjs.ml` is split into:

1. **Per-type fragment**: the JS code of a class, an enum or an implemented
   abstract, plus its metadata (static initialization dependencies, `__init__`,
   used features). Cached by `Fragment(js, type)`, depending on the
   `PostProcessed` nodes of its fields, referenced native names and the global
   set (features).
2. **Per-fragment source map**, with positions relative to the fragment.
3. **Linking**: header, type order and static initialization in dependency
   (topological) order with ties broken by path, all `__init__` blocks first, concatenation, source map offsets. Linear in output size,
   without walking any AST.
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
   (anonymous) are derived from `TypeShape` and `FieldSig`.
3. **Per-function `hlopt`**: optimization cached with the fragment (the result
   only depends on the symbolic function).
4. **HL linker**: assigns indices (types, globals, strings, functions, natives)
   in a **deterministic order** (sorted by key), resolves symbols and writes
   the bytecode and debug information. Linear in code size. The generated
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
- hxb: used only for on-disk persistence (server restart) and in verification
  mode. Loading from hxb must restore fingerprints and the graph.
- User documentation: new errors (field types), macro state declarations, macro
  warnings, `incremental-verify` and `incremental-check-refs` defines.

## 5. Phase ordering and dependencies

```
Phase 0 ─┬──────────────────────────────────────────────────────┐
Phase 1 ─┤                                                      │
         ├─ Phase 2 ─ Phase 3 ─ Phase 4 ─ Phase 5 ─ Phase 6 ─┬─ Phase 8 ─┬─ Phase 9 (JS)
         │                                                    │           └─ Phase 10 (HL)
         │                                      Phase 7 ──────┘
         └─ (continuous verification on the whole corpus)                 Phase 11
```

Phase 3 (demand-driven typing) is useful on its own: it removes order
dependence and enables parallel typing even without the server. Phase 2 comes
first because queries create and fill entities through the stable-key index.
Phase 7 (macros) starts after phase 5 (same recording mechanism) and must be
complete before enabling phase 6 on Heaps, hxbit and domkit.

## 6. Risks and open points

- **Size of the phase 3 refactor.** About 1,400 direct reads of entity
  contents to migrate behind abstract types. Mitigation: mechanical, module by
  module migration with a tracked escape hatch, and the cold-compilation
  equivalence check on the corpus after each step.
- **Macro code depending on pass order.** Macros that assume every class is
  built when they run, or that rely on cycles currently resolved by retries,
  will break with demand-driven typing. To be measured on the corpus early in
  phase 3 with the cycle instrumentation described in phase 3.3.
- **Parallel typing and the macro interpreter.** Macro expansions are
  serialized at first; this can limit parallel speedups on macro-heavy code.
- **Dependency completeness.** The main risk: a missing dependency yields a
  silently wrong result. Mitigation: the "trace only" mode of phase 5 and
  history replay in CI.
- **Memory cost** of the graph: estimated at a few tens of bytes per edge. To
  be measured in phase 5 on Heaps; if needed, compact per module (integer
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
- `tests/misc`: phase 1 errors (non-inferred field types) and phase 3 cycle
  errors (headers, build macros, recursive inferred types).
- Order-independence tests for phase 3: compile with queries demanded in
  shuffled order (`-D typer-shuffle=<seed>`) and in parallel, and compare.
- `tests/incremental`: replay harness (phase 0) on `tests/unit`, Heaps, hxbit
  and domkit.
- Unit tests for the hasher and grafting (old ↔ new mapping, remapping, stale
  pointer checker).
