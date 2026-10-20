# Incremental recompilation tests

Scenario tests for the compilation server: a project is edited step by step,
and every step is compiled twice:

- **warm**: through a compilation server (`haxe --wait`) that has compiled all
  the previous steps;
- **cold**: with a fresh compiler process, in a separate directory containing
  exactly the same files.

The main check is **equivalence**: exit code, compiler messages, program output
and every generated file must be identical between warm and cold. Step
expectations (`@expect-*`) also check that each scenario exercises what it is
meant to. See [docs/incremental-compilation.md](../../docs/incremental-compilation.md)
for the design these tests support.

## Running

```
cd tests/incremental
haxe run.hxml                         # all cases, all targets
haxe run.hxml --filter macros/        # cases whose path contains "macros/"
haxe run.hxml --targets interp        # one target
haxe run.hxml --show-known --verbose  # details of known failures, every compilation
```

Options: `--haxe <path>` (compiler to test), `--strict-order` (do not sort
compiler messages before comparing), `--invalidate` (notify the server with
`server/invalidate` instead of relying on modification times), `--keep` (keep
the `work` directory).

Targets: `interp` (`--interp`, program output is the compiler output) and `js`
(`out/main.js`, run with node). Running the js output requires `node` in `PATH`.

## Writing a case

A case is a `.hxtest` file under `cases/`:

```
# Description of the scenario.
@targets interp js
@args -D some_define

=== initial
@expect-output value 1
--- Main.hx
class Main {
	static function main() {
		trace("value " + Dep.get());
	}
}
--- Dep.hx
class Dep {
	public static function get() return 1;
}

=== change the body
@expect-output value 2
--- Dep.hx
class Dep {
	public static function get() return 2;
}
```

- Header directives: `@targets`, `@args` (extra compiler arguments), `@main`
  (`none` for no `-main`), `@known-failure [targets] <reason>`, `@real-mtime`.
- Each `=== name` section is a step: directives first (`@args` replaces the
  extra arguments from that step on), then file blocks `--- path` (content up
  to the next `--- ` or `=== ` line) and `--- delete path`.
- Expectations: `@expect-success`, `@expect-error <text>`,
  `@expect-warning <text>`, `@expect-no-warning`, `@expect-output <text>`,
  `@expect-no-output <text>`, `@expect-compiler-output <text>`,
  `@expect-file <path> <text>`, `@expect-no-file-content <path> <text>`.

## Modification times

The server compares file modification times with a precision of one second, so
edits made within the same second as the previous compilation are not detected
(`cases/basic/same-second-edit.hxtest`). Unless a case uses `@real-mtime`, the
runner gives every step its own modification time, two seconds after the
previous one, on changed files and their directories.

## Macro file dependencies

Files read by macros are not tracked automatically: a macro whose result
depends on a file must declare it with `Context.registerModuleDependency`
(see `cases/macros/macro-reads-file-registered.hxtest` and
`macro-registered-file-outside-classpath.hxtest`). Cases must not read
undeclared files from macros, since warm and cold compilations can then
legitimately differ.

## Known failures

`@known-failure` marks cases that fail with the current compiler. The runner
reports them as `XFAIL`, and as `XPASS` (a failure of the run) when they start
passing, so that the marker is removed when a fix lands. There are two kinds:

- bugs of the current server (warm differs from cold), documented in each case;
- `cases/haxe6/`: Haxe 6 semantics from the plan, not implemented yet.
