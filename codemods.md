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
- Warnings coming from libraries can be capped (follow-up).
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
}
```

They are collected per compilation (`part_scope.fixes`). Sources of fixes:

1. **Declared replacements**: `@:deprecated` on a function takes the replacement as second argument.
   The function's argument names and `this` refer to the corresponding expressions of the call:

   ```haxe
   @:deprecated('Std.is is deprecated. Use Std.isOfType instead.', Std.isOfType(v, t))
   static function is(v:Dynamic, t:Dynamic):Bool;

   @:deprecated("Use add instead", this.add(x * 2))
   public function twice(x:Int):Int;
   ```

   A call `Std.is(a ?? 2, Float)` becomes `Std.isOfType((a ?? 2), Float)`: the source text of each
   argument is reused verbatim, and wrapped in parentheses unless it is atomic (local, constant, field
   access, call, ...).
2. **Compiler-emitted fixes** (not prototyped): language changes in Haxe 5 report a warning with a
   fix instead of an error, during at least one release. Syntax changes need the fix to come from the
   parser, since typing never sees the old syntax.
3. **Existing replaceable code** (unused variables in diagnostics) could move to the same mechanism.

Later: a safety flag on each fix (Rust's machine-applicable vs maybe-incorrect), with only safe fixes
applied by default.

### Applying

- `-D apply-fixes` applies the fixes after typing and filters, if there were no errors. Which files get
  changed is selected by its value, a comma-separated list of:
  - class path kinds: `user` (`-cp`), `lib` (libraries), `std` (std and target std), `all`;
  - libraries by name: `lib:name`, e.g. `-D apply-fixes=user,lib:mylib` for a library maintainer
    compiling the library's tests, without touching other dependencies;
  - paths to directories or files, e.g. `-D apply-fixes=src/legacy`.

  Without value it is `user`: library and std files are left alone by default.
- Library names come from `haxelib path`, which prints `-D name=version` right after each library's
  class path. The compiler now records that name on the class path (`ClassPath.Lib of string option`,
  internal argument `-libcp-of name path`).
- Libraries resolved by lix are added as plain `-cp` through `haxe_libraries/*.hxml`, so they count as
  user class paths and can't be selected by name. Recognizing them needs another marker in those hxml
  files (open question).
- Overlapping fixes (a deprecated call nested in another one) keep the outermost one, and the compiler
  says to compile again. Each pass converges.
- A dedicated CLI argument (`--apply-fixes`) could replace the define once the design settles.

### IDE

Fixes are added to diagnostics as `DKReplacableCode` (kind 3, with `newCode` and `range`), which vshaxe
already turns into quick fixes. No change is needed on the vshaxe side.

## Haxe-specific issues

- **Positions count UTF-8 code points**, not bytes. Reading and writing source text needs conversion
  (`SourceText.byte_range`).
- **Conditional compilation**: only the active branches get typed. A full migration needs one run per
  define set (Rust has the same issue with `cfg`). Inactive blocks can only get parser-level fixes.
- **Macros**: generated expressions carry the call's or the macro's positions. A fix is only made if
  every argument position lies inside the call's range and differs from it. Later: verify that the text
  at the range is what the fix expects.
- **Typer-inserted arguments**: omitted optional arguments are filled with an expression at the call's
  position. They make the call not fixable for now; a better answer is to drop the argument if the
  template's matching parameter is optional too.
- **Inline functions**: a call to a deprecated `inline` function is inlined during typing, before the
  deprecation filter sees it. The fix has to be made at inline time (`CallUnification`, where
  `DeprecationCheck.check_cf` is already called). Not prototyped.
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
  `(message:String, ?replacement:Expr, ?since:String)` validates the arguments, instead of the compiler
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

- `src/context/common.ml`: `fix` type, `part_scope.fixes`.
- `src/context/display/deprecationCheck.ml`: `add_replacement_fix` on `TCall` of a deprecated static or
  instance field with a template.
- `src/context/sourceText.ml`: code point to byte offsets, reading a position's source text.
- `src/compiler/applyFixes.ml`: file selection (class path kinds, libraries, paths), overlap handling,
  writing files.
- `src/compiler/highLevel.ml`, `src/compiler/args.ml`, `src/core/classPath.ml`: library names on class
  paths.
- `src/context/display/diagnosticsPrinter.ml`: fixes as `DKReplacableCode`.
- `std/Std.hx`: `Std.is` replacement.
- Test: `tests/misc/eval/projects/apply-fixes`: copies sources, registers a haxelib dev library, applies
  twice and compares. An unselected library is untouched, the one selected with `lib:name` is fixed.
- Unit macro, misc (719) and server tests pass.

## Next steps

1. Cap deprecation warnings from `Lib` class paths, with a way to show them again.
2. Fixes for deprecated `inline` functions.
3. Omitted optional arguments.
4. Safety flag; text check before applying.
5. Replacement templates on the other `@:deprecated` std fields, and fixes for Haxe 5 language changes.
6. Decide between the define and a CLI argument.
7. Library names for lix-resolved libraries.
