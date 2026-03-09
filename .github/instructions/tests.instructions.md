---
description: "Use when adding or modifying tests in tests/**, especially regression tests, target-specific tests, or expected-failure coverage."
name: "Haxe Test Conventions"
applyTo: "tests/**"
---
# Haxe Test Conventions

## Test Placement
- Put successful bug-fix regressions in `tests/unit/src/unit/issues/Issue{NUMBER}.hx`.
- Put tests that are expected to fail in `tests/misc/` (or a target-specific subdirectory under it).
- Put tests that interact with the compilation server or are related to completion/display in `tests/server`.
- Put optimization tests that check generated JS code in `tests/optimization`.
- Keep target-restricted behavior under explicit `#if target` guards.

## Regression Pattern
- Use one file per issue to avoid merge conflicts and keep intent obvious.
- Name regression classes `Issue{NUMBER}` in package `unit.issues`.
- Keep assertions focused on the bug behavior, not broad feature retesting.

## Running Tests
- Compile a target with `haxe --cwd tests/unit compile-{target}.hxml`.
- Run the produced test artifact with the correct runtime (for example `lua bin/unit.lua` or `node bin/unit.js`).
- If behavior is generator-related, inspect generated output for the affected target.

## Practical Checks Before Finishing
- Confirm the new test fails without the fix and passes with the fix.
- Confirm conditional compilation only gates what is truly target-specific.
- Avoid moving unrelated tests or reformatting large test files in the same change.
