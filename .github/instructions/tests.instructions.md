---
description: "Use when adding or modifying tests in tests/**, especially regression tests, target-specific tests, or expected-failure coverage."
name: "Haxe Test Conventions"
applyTo: "tests/**"
---
# Haxe Test Conventions

## Test Placement
- Put successful bug-fix regressions in `tests/unit/src/unit/issues/Issue{NUMBER}.hx`.
- Never put anything that creates threads or uses `sys.thread` into the unit tests. Such tests go in `tests/threads/src/cases/`, as a `testIssue{NUMBER}_description` method in the matching case class (e.g. `TestThread`, `TestMutex`).
- Never put anything that spawns processes (`sys.io.Process`, `Sys.command`) into the unit tests. Such tests go in `tests/sys/src/`, as a `testIssue{NUMBER}_description` method in the matching class (e.g. `io/TestProcess.hx`).
- Put tests for compiler errors and warnings, and anything that needs its own hxml, in a project under `tests/misc/eval/projects/Issue{NUMBER}/` (or `tests/misc/{target}/projects/` for target-specific ones). See "Misc Projects" below.
- Put tests that interact with the compilation server or are related to completion/display in `tests/server`.
- Put optimization tests that check generated JS code in `tests/optimization`.
- Keep target-restricted behavior under explicit `#if target` guards.

## Regression Pattern
```haxe
package unit.issues;

class Issue12345 extends unit.Test {
	#if target_name // Use conditionals for target-specific tests
	function test() {
		eq(actualValue, expectedValue);
	}
	#end
}
```
- Use one file per issue to avoid merge conflicts and keep intent obvious.
- Name regression classes `Issue{NUMBER}` in package `unit.issues`.
- Keep assertions focused on the bug behavior, not broad feature retesting.
- Test methods must start with `test`. `unit.Test` provides `eq`, `feq`, `t`, `f`, `exc`, `unspec`, `noAssert`, `hf`/`nhf`/`hsf`/`nhsf` (has (static) field) and `assert` for failures; use `utest.Assert` for anything else. Issue classes are picked up automatically, nothing needs registering.

## Misc Projects
- `compile.hxml` must compile successfully (exit code 0).
- `compile-fail.hxml` must fail, and the compiler's stderr must match `compile-fail.hxml.stderr` exactly.
- Multiple cases per project are possible by numbering: `compile1.hxml`, `compile2-fail.hxml` + `compile2-fail.hxml.stderr`, etc.
- Paths in the expected output are relative to the project directory (e.g. `Main.hx:3: characters 76-78 : Object requires fields: foo, bar`); positions inside the standard library are normalized by the runner. Create the file from the compiler's actual output after checking it.
- See `tests/misc/README.md` for details.

## Running Tests
Unit tests require the `utest` library (see `extra/agents/setup.md` if it is missing). The macro and JavaScript unit suites take under ten seconds each, so run them whole; C++ takes minutes. The JavaScript tests are good for checking output syntax.

```bash
# Unit tests: compile for a specific target (output goes to tests/unit/bin/)
haxe --cwd tests/unit compile-{target}.hxml

# Then run from tests/unit, e.g.:
cd tests/unit
node bin/unit.js       # JavaScript
lua bin/unit.lua       # Lua
neko bin/unit.n        # Neko
python3 bin/unit.py    # Python
hl bin/unit.hl         # HashLink

# The macro (interpreter) target runs the tests during compilation, no separate run step:
haxe --cwd tests/unit compile-macro.hxml

# Run only the tests whose class or method name matches a pattern (utest):
haxe --cwd tests/unit compile-macro.hxml -D UTEST_PATTERN=Issue1234

# Misc tests, optionally filtered to a single project (Issue1234):
haxe --cwd tests/misc -D MISC_TEST_FILTER=1234 compile.hxml
# Target-specific misc tests:
haxe --cwd tests/misc/{target} -D MISC_TEST_FILTER=1234 run.hxml

# Other suites on the interpreter; see tests/runci/targets/*.hx for the other targets
haxe --cwd tests/nullsafety test.hxml
haxe --cwd tests/optimization run.hxml
haxe --cwd tests/threads build.hxml --interp
cd tests/sys && haxe compile-eval-hxb.hxml && EXISTS=1 haxe --hxb-lib bin/eval/sys.hxb --run Main
cd tests/server && haxe run.hxml    # about two minutes; needs node, hxnodejs, hxcoro, haxeserver
```

- `--cwd` does not apply to `--cmd` lines in an hxml, so suites whose hxml also runs the output (like `tests/server/run.hxml`) must be run from their own directory.
- To run the full CI test suite for one or more targets, set the `TEST` environment variable (e.g. `TEST=js,macro`) and run `haxe RunCi.hxml` from `tests/`. See `tests/README.md`; `tests/unit/README.md` has the unit test guidelines.
- If behavior is generator-related, inspect generated output for the affected target.

## Practical Checks Before Finishing
- Confirm the new test fails without the fix and passes with the fix.
- Confirm conditional compilation only gates what is truly target-specific.
- Avoid moving unrelated tests or reformatting large test files in the same change.
