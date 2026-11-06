# Code migrations ("codemods") for Haxe

Status: design draft, prototype on `feature/apply-fixes`.

## Goal

When Haxe (or a library) deprecates or changes an API, users should be able to update their code with
one command instead of editing every call site by hand. The same fixes should show up as quick fixes in
the IDE. Haxe 5 is the first target: everything it deprecates or removes should come with an automatic
fix where one is possible.

## Prior art

| Approach | Examples | Notes |
|---|---|---|
| External scripts over the syntax tree | React codemods (jscodeshift), Python `2to3` / LibCST, ts-morph | Easy to write, but little or no type information. |
| Compiler suggests fixes, a tool applies them | Rust `cargo fix --edition`, clang `-fixit` / clang-tidy, Scala 3 `-source 3.0-migration -rewrite`, Swift migrator | Exact text edits attached to warnings. Rust tags each one as machine-applicable or "maybe incorrect"; `cargo fix` only applies the former. |
| Library authors declare the replacement | Kotlin `@Deprecated(replaceWith = ReplaceWith(...))`, Dart `dart fix` + `fix_data.yaml`, Go `//go:fix inline` | Works for third-party APIs, not only the standard library. |
| Typed recipe engines | OpenRewrite (Java), Scalafix, Roslyn code fixes, Angular `ng update` schematics | Typed rules, often run automatically on version upgrades. |

What the successful ones share:

1. They use type information (resolved symbols, not text patterns).
2. They edit source ranges instead of reprinting files, so formatting and comments survive.
3. The old code keeps compiling while the fix is suggested; the fix is the migration path, not the error.

### Dependencies

No ecosystem rewrites library code. Rust caps lints of registry crates (`--cap-lints=allow`) and `cargo fix`
only edits workspace crates; Go only rewrites the main module; Dart only reports diagnostics and applies
fixes for the current package; JS codemods skip `node_modules`; C/C++ silences `-isystem` headers.
Breaking changes in dependencies are handled by per-module language versions (Rust editions, the Go
directive in `go.mod`, Dart language versions, Swift 6 language mode) or by precompiled libraries.

Haxe compiles libraries from source with the user's compiler, so neither applies. Per-library language
versions are not realistic for Haxe (see Rejected alternatives). What remains:

- Fixes are only applied to user code by default, with a bypass for library and std maintainers.
- Deprecation warnings located in libraries are hidden by default (`-w +WDeprecated@lib` shows
  them, and `--fix` always does).
- Breaking changes keep a deprecation period with a fix, so library authors can migrate, and
  `#if (haxe >= version("..."))` covers code that has to support both compiler versions.

## Design

### Fixes

A fix is a text edit attached to a compiler message:

```ocaml
type fix = {
	fix_reason : string;
	fix_range : pos;
	fix_text : string;
	fix_since : string option; (* oldest Haxe version accepting fix_text *)
}
```

They are collected per compilation (`part_scope.fixes`). Sources of fixes:

1. **Declared replacements**: `@:deprecated` takes the replacement as second argument. On a function,
   it replaces calls, and the function's argument names and `this` refer to the expressions of the
   call. On a variable or property, it replaces accesses, with `this` referring to the receiver. An
   optional third argument gives the oldest Haxe version accepting the replacement:

   ```haxe
   @:deprecated('Std.is is deprecated. Use Std.isOfType instead.', Std.isOfType(v, t))
   static function is(v:Dynamic, t:Dynamic):Bool;

   @:deprecated("Use high instead", x.high)
   public static inline function getHigh(x:Int64):Int32;

   @:deprecated("Use haxe.EventLoop.getThreadLoop(thread) instead", haxe.EventLoop.getThreadLoop(this), "5.0.0")
   public var events(get, null):Null<haxe.EventLoop>;
   ```

   The source text of each argument is reused verbatim. It is wrapped in parentheses where the template
   needs it: `Int64.getHigh(i + i)` becomes `(i + i).high`, but `Std.is(a ?? 2, Float)` becomes
   `Std.isOfType(a ?? 2, Float)`. An omitted optional argument that the template passes through as a
   trailing call argument is dropped (`Old.create(1)` becomes `Old.make(1)`); any other use of it makes
   the call not fixable. Calls to deprecated `inline` functions get their fix before inlining.

   The std deprecations with a direct replacement have templates, in every target's std: `Std.is`,
   `Std.instance`, `Int64.is`/`getHigh`/`getLow`, `Sys.executablePath`, `StringTools.quoteUnixArg`/
   `quoteWinArg`/`winMetaCharacters`, `Bytes.readString`, `Thread.events`, `EventLoop.repeat`/`cancel`,
   the coro `Context.add`, `php.Boot.is`.
2. **Deprecated typedef aliases**: a deprecated typedef that only renames a type, with the same type
   parameters, needs no template: its uses in type hints are replaced by the aliased type's path
   (`js.Error` becomes `js.lib.Error`, `js.Error.RangeError` becomes `js.lib.Error.RangeError`).
3. **Fixes on errors**: where Haxe 5 rejects code Haxe 4 accepted, the error carries a fix computed
   from the old behavior. For example, a duplicate argument name renames the first argument, which the
   function body could never refer to, to `_`.
4. **Migration warnings**: where code still compiles but means something else, a warning of the
   `WMigration` family carries a fix that keeps the Haxe 4 meaning (see Haxe 5 breaking changes).
   They are disabled by default, since they keep firing on code that is correct for Haxe 5 (a class
   meant to win over an enum constructor, a library already adapted to the new build order), and
   their checks cost time. `--fix` enables them below the user's `-w` options, so
   `-w -WBuildMacroOrder` still disables one; `-w +WMigration` enables them without the codemod.
5. **Syntax deprecations**: `@:final`, `@:extern` and `@:enum abstract` are replaced by the keyword,
   placed before `function`, `var` or `abstract` (`@:final public var x` becomes `public final x`).
6. **Hints without fix**, for what can't be rewritten: modules moved to the hx4compat library or removed
   (`sys.db`, `haxe.remoting`, `php.Web`, `neko.Web`, `haxe.Ucs2`), and the removed
   `haxe.macro.Compiler` functions (`removeField`, `setFieldType`, `patchTypes`).
7. **Existing replaceable code** (unused variables in diagnostics) could move to the same mechanism.

Fixes attached to warnings are only made when the warning is enabled at that position, so disabling a
warning (`-w`, `@:haxe.warning`) also keeps its fix from being applied.

Each fix is safe or unsafe (Rust's machine-applicable vs maybe-incorrect, Ruff's safe and unsafe
fixes). `--fix` only applies safe fixes; `--fix-unsafe` applies both, and the compiler says how many
unsafe fixes were skipped otherwise. Unsafe fixes:

- Templates that duplicate or drop an argument with side effects: `@:deprecated(.., Old.twice(x, x))`
  is safe on `Old.double(a)` but not on `Old.double(next())`, where `next()` would run twice. Arguments
  are pure when they are constants, locals, type expressions, functions, or field accesses on those.
- Renamed fields (`is`, see Haxe 5 breaking changes): uses the compiler doesn't type (reflection,
  `Dynamic` accesses, code in other `#if` branches, other projects using the library) are missed.

### Applying

| Argument | |
|---|---|
| `--fix` | Apply the fixes to the files of the user class paths |
| `--fix-scope <scopes>` | Comma-separated: `user`, `lib`, `lib:name`, `std`, `all`, `path:path` or `pack:dot.path` |
| `--fix-diff` | Print a unified diff instead of changing files |
| `--fix-passes <n>` | Compile again while fixes were applied, up to n compilations |
| `--fix-compat <version>` | Skip fixes needing a newer Haxe version |
| `--fix-allow-dirty` | Also change files with uncommitted changes or outside of git |
| `--fix-unsafe` | Also apply unsafe fixes |
| `--fix-only <warnings>` | Only apply the fixes of these warnings or warning groups, and of errors |

Each argument enables fixes, and their order doesn't matter (`--fix-scope lib --fix` keeps the scope).
They are not defines: these would change the context signature, so a `--fix` compilation couldn't
reuse the compilation server's cache of a normal one. Macros get the settings from
`haxe.macro.Compiler.getConfiguration().fix` (`null` without fixes).
The prior art for the names: Rust's `cargo fix` (`--allow-dirty`, `--broken-code`), Go's `go fix -diff`,
ESLint's `--fix`/`--fix-dry-run`, Ruff's `--fix`/`--diff`/`--unsafe-fixes`, Dart's
`dart fix --apply`/`--dry-run`, clang-tidy's `-fix`/`-fix-errors`.

- Fixes are applied at the end of the compilation, including failed ones, so errors can carry fixes.
- Files with uncommitted changes, or outside of a git repository (including ignored files), are not
  changed: nothing is applied, and an error lists them. `--fix-allow-dirty` overrides it; `--fix-diff`
  doesn't need it.
- Scopes: `user` (`-cp`), `lib` (libraries), `lib:name` (one library, e.g. `--fix-scope user,lib:mylib`
  for a library maintainer compiling the library's tests), `std` (std and target std), `all`, directories or
  files (`path:src/legacy`, or just `src/legacy`), packages or modules (`pack:my.legacy`), as in warning
  scopes. The default is `user`.
- Library names come from `haxelib path`, which prints `-D name=version` right after each library's
  class path. The compiler records that name on the class path (`ClassPath.Lib of string option`,
  internal argument `-libcp-of name path`).
- Libraries resolved by lix are added as plain `-cp` through `haxe_libraries/*.hxml`, so they count as
  user class paths and can't be selected by name (open question).
- Overlapping fixes (a deprecated call nested in another one) keep the outermost one, and the compiler
  says to compile again. Each pass converges.
- Typing stops at the first error, so a pass fixes at most one error, plus the warnings found before it.
  `--fix-passes` is opt-in: a failed compilation leaves nothing in the cache, so each pass after an
  error is a full compilation, and init macros run again on each pass. The cache of changed files is
  invalidated between passes.
- `--fix-diff` paths are as the compiler knows them, so the diff applies with `git apply -p0` or
  `patch -p0`. Only the first pass can be shown, since later passes depend on the files changing.
- `--fix-only` takes comma-separated warning names, groups included (`--fix-only WMigration`), like
  Ruff's `--select` or Dart's `--code`. Fixes of errors are always applied: the code doesn't compile
  without them.
- Files changed since they were parsed (by an editor, or a macro) are not changed, and the compiler
  says how many fixes were skipped: their positions may not match anymore. Each fix keeps the file
  time of its module (taken when the module was loaded, as the compilation server does), compared
  with the file's time when applying.
- With the compilation server, modules served from the cache aren't typed again. Fixes are bound to
  the module being typed when they are recorded, like warnings (`m_cache_bound_objects`), and replayed with it, checking the
  current `-w` options again.
- A fix can require other files to be in scope: a renamed field's uses are only renamed when all the
  declarations of the field (overridden and implemented ones included) can be renamed too.

### Warning scopes

`-w` accepts a scope after a warning, to apply to warnings located in some files only:

```
-w -WDeprecated@lib              # all libraries
-w +WDeprecated@lib:tink_core    # one library
-w -WIfDisplay@std               # std and target std
-w +WDeprecated@user             # -cp class paths
-w -WDeprecated@path:src/gen    # a directory or file
-w -WDeprecated@pack:my.gen      # a package (and its subpackages) or a module (my.gen.Types)
```

- A scope value (`lib:`, `path:`, `pack:`) ends at whitespace, and can be quoted (`@path:"my dir"`).
  The parser doesn't use the Haxe lexer, so library names and paths can contain any character.
- Library names match regardless of `-` and `_` (`@lib:haxeui-core` and `@lib:haxeui_core`), as
  defines do.
- `pack:` takes a package or a module dot path, like `Compiler.keep` and `Compiler.nullSafety`. The
  package of a file comes from its path relative to its class path. Sub-types can't be selected, since
  warnings are located by file: `pack:my.Mod.Sub` is an error.
- `@:haxe.warning` doesn't accept scopes: the metadata already applies to one class or field.

A scoped option takes precedence over options without scope. Deprecation warnings located in libraries
and the standard library are disabled by default (`-WDeprecated@lib -WDeprecated@std`): users can't act
on them. `-w +WDeprecated@lib` reports them again, and `--fix` enables them below the user's own scoped
options (`--fix -w -WDeprecated@lib` still hides them). Other ecosystems: GCC's `-isystem`, Rust's
`--cap-lints` on dependencies, ESLint rules scoped by file globs.

This part is independent from the fixes, and is on its own branch on top of `development`
(`feature/warning-scopes`), to be upstreamed first. Changes to the scope code are made on both
branches.

### IDE

Fixes are added to diagnostics as `DKReplacableCode` (kind 3, with `newCode` and `range`), which vshaxe
already turns into quick fixes. No change is needed on the vshaxe side.

## Haxe 5 breaking changes

From the 5.0.0-preview.1 changelog, compared with Haxe 4.3.7:

| Change | 4.3.7 | Haxe 5 | Fix | Safe | Status |
|---|---|---|---|---|---|
| Partial resolution (#11338): `import pack.Mod; pack.SubType.x` | compiles | error `Unknown identifier : pack` | Retry the old lookup on failure, fix to `pack.Mod.SubType`, in type hints and expressions | yes | done |
| Duplicate argument names (#11978) | compiles | error | Rename the first one to `_` | yes | done |
| Enum constructors no longer win over classes (#9197) | enum constructor | type, silently | `WEnumConstructorShadowed`, rewrite to `E.Foo` | yes | done |
| `bind` with optional arguments (#11533): `f.bind(1)` | `() -> Int` | `(?b, ?c) -> Int`, silently | None: the Haxe 5 type still unifies with `() -> Int`, and the explicit form `f.bind(1, null, null)` doesn't compile with Haxe 4 on static targets for arguments with a default value | | won't fix |
| No String inferred from concatenation (#11318): `var a = null; a + "x"` | `Null<String>` | `Null<Unknown>` | None: Haxe 5 accepts strictly more in the cases tested (later uses still infer the type) | | won't fix |
| Build macro order (#11582): inherited `@:autoBuild` macros run in reverse order | | | `WBuildMacroOrder`: which macro changed which fields before another one ran. No fix: the library's macros have to change | | done |
| Removed `Compiler` API (#11540) | | error | Hint on the error | | done |
| Typer creation after init macros, enum class fields, types in existing modules | | | Too varied for rewrites | | |
| Moved to `hx4compat` (`sys.db`, `haxe.remoting`, `php.Web`, `neko.Web`), `haxe.Ucs2` | | error | Hint on the error (not in expressions yet) | | done |
| Java/C# targets | | | Not a source edit | | |

Deprecations still warning in Haxe 5 are handled too: `@:final`, `@:extern` and `@:enum abstract`, the
std `@:deprecated` fields with a direct replacement, and deprecated typedef aliases (see Fixes).

`is` as an identifier (only warned with `-D haxe-next`) is renamed to `isValue` (or `isValue2`, ...,
avoiding the names in use):

- Locals and arguments, with their uses (safe).
- Fields, with the uses the compiler types (unsafe). Fields that override or implement each other are
  renamed together, and only when none is extern or `@:native` and all are in the fix scope: a class
  overriding a library's `is` keeps it. Fields of anonymous structures and `import Foo.is` are not
  renamed.

The enum constructor case shows the general pattern: the fix is made where Haxe 5 decides differently,
by computing what Haxe 4 would have done there. Identifiers heading a field access (`Foo.v`) are not
checked, since they didn't compile with Haxe 4 when `Foo` was an enum constructor.

### Case study: wisdom

[wisdom](https://github.com/jeremyfa/wisdom) (`build-test.hxml`) compiles with Haxe 4.3.7 but fails with
Haxe 5 with eleven `Missing return: wisdom_.VNode` errors on `function render() '<>...'` methods. The
parsed bodies are identical; the cause is #11582. Components inherit two build macros from
`wisdom.Component`, `ComponentMacro` and `XMacro` (from `implements X`), and Haxe 5 runs `XMacro` first.
`ComponentMacro` only adds the `return` when the body is still a markup string, and `XMacro` has
already converted it.

`--fix` changes nothing there: no source rewrite can undo a macro ordering change. The
`WBuildMacroOrder` warning reports the cause instead:

```
Haxe 4 ran the build macros inherited from wisdom.Component in the reverse order:
tracker.macros.ObservableMacro.build changed __cbOnHostsChange, ... before wisdom.XMacro.build ran
wisdom.XMacro.build changed render before wisdom.ComponentMacro.build ran
```

The library fix is to make `ComponentMacro` accept both forms (`XMacro` marks converted markup with
`@:wisdomMarkup`). It compiles and passes the regression suite with both 4.3.7 and Haxe 5.

The warning compares the printed fields before and after each macro of a parent that has several
inherited build macros, and is emitted once per parent type, only when such an interaction exists.

### Case study: buddy

buddy's `tests.hxml`, with the tink libraries of its local `.haxelib`, fails with Haxe 5 on two
partial resolutions in tink_macro (`tink.OutcomeTools`, `haxe.macro.Error`). `--fix-scope all` fixes
both (`tink.CoreApi.OutcomeTools`, `haxe.macro.Expr.Error`); the remaining errors are then the same as
with Haxe 4.3.7 (those tink versions don't build with 4.3.7 either).

### Haxe 4.3.x

4.3.x doesn't know the new rules, so the fixes ship with Haxe 5. A 4.3.x release is also planned to add
some of these warnings behind `-D haxe-next` (the define already exists), so code can be migrated before
upgrading.

## Compatibility with older Haxe versions

Libraries usually have to keep compiling with Haxe 4 while they migrate, so a fix must not introduce
code that only Haxe 5 accepts, unless the user says so.

- Each fix can declare the oldest Haxe version accepting its replacement (`fix_since`); templates give
  it as third `@:deprecated` argument.
- `--fix-compat 4.3` gives the oldest version the code must keep compiling with. Fixes needing
  a newer version are skipped, with a note telling how many and which versions they need. Without it,
  the fixes target the current compiler.
- Later, a too recent fix could be replaced by an alternative instead of skipped: a different
  replacement older versions accept, or `#if (haxe >= version("5.0.0")) new #else old #end` as a last
  resort.

All the fixes so far are compatible with Haxe 4 except `Thread.events` (`haxe.EventLoop` is new in
Haxe 5): `Std.isOfType`, `Std.downcast`, `haxe.SysTools`, `Sys.programPath` and `js.lib.*` exist in 4.3,
`_` arguments, qualified enum constructors, `pack.Module.SubType`, `final`/`extern`/`enum abstract` are
valid in 4.x, and the migration fixes write the Haxe 4 meaning explicitly.

## Haxe-specific issues

- **Positions count UTF-8 code points**, not bytes. Reading and writing source text needs conversion
  (`SourceText.byte_range`).
- **Conditional compilation**: only the active branches get typed. A full migration needs one run per
  define set (Rust has the same issue with `cfg`). Inactive blocks can only get parser-level fixes.
- **Macros**: generated expressions carry the call's or the macro's positions. A fix is only made if
  every argument position lies inside the call's range and differs from it. Later: verify that the text
  at the range is what the fix expects.
- **Typer-inserted arguments**: omitted optional arguments are filled with an expression at the call's
  position, which is how they are recognized (see Fixes for how templates handle them).
- **Inline functions**: a call to a deprecated `inline` function is inlined during typing, before the
  deprecation filter sees it, so its fix is made in `Calls`, before inlining.
- **Lazy typedefs**: a typedef's type may not be resolved yet when a type hint loads it, so the alias
  fix is delayed to a later typer pass.
- **Inline and `@:generic`**: the same code is typed several times; fixes are deduplicated by range.
- **Hygiene**: the template is printed as is, so its identifiers resolve at the call site. A local
  named `Std` would capture `Std.isOfType`. Typed metadata solves this (below).
- **Formatting**: the template is printed with `Ast.Printer`, so it uses its formatting. Only the call's
  range is replaced; arguments keep their source text.

## With typed metadata

Specs aren't ready, but if metadata gets declared signatures, `@:deprecated` improves a lot:

- **Checked templates**: the replacement could be typed when the deprecated function is declared, with
  its arguments and `this` in scope. A wrong template becomes an error for the library author, instead
  of broken rewrites in users' code.
- **Hygiene**: identifiers in the typed template are resolved at the declaration. The fix can then print
  fully qualified paths, or add the imports it needs, instead of relying on the call site's scope.
- **Structured arguments**: a declared signature such as
  `(message:String, ?replacement:Expr, ?since:String)` validates the arguments (`since` being the
  minimum Haxe version of the replacement, see Compatibility), instead of the compiler
  guessing from positional expressions.
- **Library-defined migrations**: libraries could declare their own typed metas that the compiler
  recognizes as fix providers, e.g. for renamed types (`@:deprecated(..., NewName)` on a typedef or
  class) or moved fields.
- **IDE support**: completion and hover inside meta arguments come for free with typed arguments.

## Rejected alternatives

- **Per-library language versions** (Rust editions, Dart language versions): every breaking change
  would have to stay switchable per file, which the Haxe compiler can't support.
- **External codemod tool**: it would need to reimplement type resolution. Fixes from the compiler get
  typing, macros and the IDE integration for free.

## Prototype (`feature/apply-fixes`)

- `src/context/common.ml`: `fix` type, `part_scope.fixes`, class path scope lookup, library deprecation
  capping.
- `src/context/sourceText.ml`: code point to byte offsets, reading a position's source text, meta to
  keyword rewrites.
- `src/context/display/deprecationCheck.ml`: templates on calls and accesses, `is` renames.
- `src/typing/calls.ml`: templates on inline calls.
- `src/compiler/applyFixes.ml`: file selection (class path kinds, libraries, paths), compatibility
  target, safety, `--fix-only`, files changed during the compilation, overlap handling, writing files,
  dry mode (unified diff built from the edit ranges).
- `src/compiler/highLevel.ml`, `src/compiler/args.ml`, `src/core/classPath.ml`: library names on class
  paths, passes.
- `src/compiler/compiler.ml`: applying after failed compilations, enabling `WMigration`.
- `src/core/warning.ml`, `src/core/classPaths.ml`, `src/compiler/io/compilerMessage.ml`: warning scopes.
- `src/compiler/args.ml`, `src/compiler/parsedArg.ml`: `--fix` arguments.
- `src/context/display/diagnosticsPrinter.ml`: fixes as `DKReplacableCode`.
- `src/typing/functionArguments.ml`: duplicate argument names.
- `src/context/resolution.ml`, `src/typing/typer.ml`: `WEnumConstructorShadowed`.
- `src/typing/typeloadFields.ml`: `WBuildMacroOrder`, `@:final`/`@:extern`.
- `src/typing/typeloadModule.ml`: `@:enum abstract`, moved and removed modules.
- `src/typing/typeload.ml`, `src/typing/typerDotPath.ml`: partial resolution, deprecated typedef aliases.
- `src/typing/fields.ml`: removed `Compiler` functions.
- `std/**`: replacement templates.
- Tests in `tests/misc/eval/projects/`: `apply-fixes` (selection, libraries), `apply-fixes-migration`,
  `apply-fixes-dry`, `apply-fixes-partial-resolution`, `apply-fixes-syntax`, `apply-fixes-std`,
  `apply-fixes-alias`, `apply-fixes-compat`, `apply-fixes-is`, `apply-fixes-is-fields`,
  `apply-fixes-safety` (unsafe templates, changed files), `apply-fixes-config`, `apply-fixes-dirty`,
  `build-macro-order-warning`, `lib-deprecation-warnings`, `warning-scopes`,
  `removed-modules`.
- `tests/server/src/cases/ApplyFixes.hx`: fixes of cached modules.
- Unit macro, misc (783) and server tests pass.

## Next steps

1. Library names for lix-resolved libraries.
2. 4.3.x backport behind `-D haxe-next`.
