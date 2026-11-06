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
- Put tests for compiler errors and warnings, and anything that needs its own hxml, in a project under `tests/misc/eval/projects/Issue{NUMBER}/` (or `tests/misc/{target}/projects/` for target-specific ones): `compile.hxml` must succeed, `compile-fail.hxml` must produce exactly the stderr in `compile-fail.hxml.stderr`. Paths in the expected output are relative to the project directory (e.g. `Main.hx:3: characters 76-78 : Object requires fields: foo, bar`); positions inside the standard library are normalized by the runner. Create the file from the compiler's actual output after checking it. See `tests/misc/README.md`.
- Put tests that interact with the compilation server or are related to completion/display in `tests/server`.
- Put optimization tests that check generated JS code in `tests/optimization`.
- Keep target-restricted behavior under explicit `#if target` guards.

## Regression Pattern
- Use one file per issue to avoid merge conflicts and keep intent obvious.
- Name regression classes `Issue{NUMBER}` in package `unit.issues`.
- Keep assertions focused on the bug behavior, not broad feature retesting.
- Test methods must start with `test`. `unit.Test` provides `eq`, `feq`, `t`, `f`, `exc`, `unspec`, `noAssert`, `hf`/`nhf`/`hsf`/`nhsf` (has (static) field) and `assert` for failures; use `utest.Assert` for anything else. Issue classes are picked up automatically, nothing needs registering.

## Running Tests
- Compile a target with `haxe --cwd tests/unit compile-{target}.hxml`; `compile-macro.hxml` runs the tests during compilation.
- Run a single test class or method with `-D UTEST_PATTERN=Issue{NUMBER}`, a single misc project with `-D MISC_TEST_FILTER={NUMBER}`.
- Run the produced test artifact with the correct runtime (for example `lua bin/unit.lua` or `node bin/unit.js`).
- If behavior is generator-related, inspect generated output for the affected target.

## Practical Checks Before Finishing
- Confirm the new test fails without the fix and passes with the fix.
- Confirm conditional compilation only gates what is truly target-specific.
- Avoid moving unrelated tests or reformatting large test files in the same change.
