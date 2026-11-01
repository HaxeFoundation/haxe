# Agent Instructions for Haxe

## Repository Overview

This is the Haxe compiler repository. Haxe is an open source toolkit that allows building cross-platform tools and applications. The repository contains:

- **The Haxe compiler** - written in OCaml, targets multiple platforms (JavaScript, C++, JVM, Lua, PHP, Python, HashLink, NekoVM, Flash, and its own interpreter)
- **The Haxe standard library** - located in `std/`, written in Haxe
- **Test suite** - located in `tests/`, particularly unit tests in `tests/unit/`

Path-scoped instructions with more detail live in `.github/instructions/`. Read the relevant one before working in its area:
- `.github/instructions/ocaml-compiler.instructions.md` - OCaml compiler sources (`src/**/*.{ml,mli}`)
- `.github/instructions/tests.instructions.md` - tests (`tests/**`)

## Build System

- **Language**: The compiler is written in OCaml (version 5.0+)
- **Build tool**: Uses `dune` (3.17+) and Make
- **Makefile**: For Unix/Linux/macOS, use `Makefile`; for Windows, use `Makefile.win`
- **Build commands**:
  - `make` - Build everything
  - `make haxe` - Build only the compiler; copies the `haxe` executable to the repository root
  - `opam install haxe --deps-only` - Install OCaml dependencies (after pinning the local checkout)
  - `opam exec -- make ADD_REVISION=1 -f Makefile.win -s -j haxe` - Typical Windows compiler build command used in CI/tasks
  - `dune build` - Build using dune directly

### Use the Compiler You Just Built
This repository may be checked out in several git worktrees, and the global PATH can point to a `haxe` binary from a different checkout. A bare `haxe` command may therefore run a different compiler than the one you just built.
- Call the compiler and haxelib from the repository root by path, e.g. `./haxe` and `./haxelib` (`.\haxe.exe` in PowerShell), or put the repository root first on PATH for the session
- When in doubt, check with `haxe -version` or `Get-Command haxe` / `which haxe`
- The `haxe` commands in this file assume this has been taken care of

## Development Setup

### Prerequisites
- OCaml 5.0+ (managed via OPAM)
- Native libraries: PCRE2, zlib, mbedTLS 3.x (4.x not yet supported)
- Neko VM (for building haxelib)

See `extra/BUILDING.md` for full instructions.

## Code Organization

### Compiler Source (`src/`)
- `core/` - Core data structures (AST, types, globals) shared by the whole compiler
- `syntax/` - Lexer and parser
- `typing/` - Type system and type inference
- `context/` - Compiler context and state
- `compiler/` - Compiler entry point, argument handling, compilation server
- `codegen/` - Code generation utilities (including AST dumping)
- `generators/` - Target-specific code generators (JVM, JS, C++, etc.)
- `optimization/` - Optimization passes
- `macro/` - Macro system
- `filters/` - AST transformation filters
- `coro/` - Coroutine transformation

### Compiler Definitions (`src-json/`)
- `define.json` - Compiler defines (`-D` flags)
- `meta.json` - Metadata (`@:meta`)
- `warning.json` - Warnings
- Add new defines, metadata and warnings here; the corresponding OCaml code is generated at build time

### Standard Library (`std/`)
- Core types (Array, String, Map, etc.) at root level
- `haxe/` and `sys/` - Platform-independent standard library packages
- Platform-specific directories (e.g., `cpp/`, `js/`, `jvm/`, `python/`, etc.)
  - Each platform can shadow standard library implementations via `platform/_std/` directory
  - Example: `std/cpp/_std/haxe/ds/StringMap.hx` shadows the generic `std/haxe/ds/StringMap.hx`

### Tests (`tests/`)
- `unit/` - Unit tests written in Haxe
  - `unit/src/unit/issues/` - Regression tests for specific issues (success cases)
- `misc/` - Project-based compiler tests, mainly for checking compiler errors and warnings
  - `misc/projects/` - Target-independent projects
  - Platform-specific subdirectories (e.g., `misc/python/`, `misc/hl/`) for target-specific projects
- `server/` - Compilation server and display/completion tests
- `nullsafety/` - Tests related to the null-safety feature
- `optimization/` - Optimization tests that check concrete code output on JavaScript target
- `sys/` - Tests specific to the `std/sys` package (sys-targets only: excludes Flash and JavaScript)
- `threads/` - Thread-related tests, generally for `std/sys/thread` package (threaded targets only)
- `RunCi.hx` and `runci/` - The script CI uses to run all test suites

## Testing Guidelines

### Adding Regression Tests

**For tests that assert success**, add a regression test to `tests/unit/src/unit/issues/`:

1. Create a file: `tests/unit/src/unit/issues/Issue{NUMBER}.hx`
2. Follow this pattern:
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

**For expected compiler errors/warnings**, add a project to `tests/misc/projects/Issue{NUMBER}/` (or the platform-specific subdirectory):
- `compile.hxml` - must compile successfully (exit code 0)
- `compile-fail.hxml` together with `compile-fail.hxml.stderr` - the compiler's stderr must match the `.stderr` file exactly
- Multiple cases per project are possible by numbering: `compile1.hxml`, `compile2-fail.hxml` + `compile2-fail.hxml.stderr`, etc.
- See `tests/misc/README.md` for details

**Note**: Not all tests belong in the unit tests. See the Tests section above for other test directories and their purposes.

The JavaScript tests are good for checking output syntax.

### Running Tests

Unit tests require the `utest` library (see the Haxelib section).

```bash
# Unit tests: compile for a specific target (output goes to tests/unit/bin/)
haxe --cwd tests/unit compile-{target}.hxml

# Then run from tests/unit, e.g.:
cd tests/unit
node bin/unit.js    # JavaScript
lua bin/unit.lua    # Lua

# The macro (interpreter) target runs the tests during compilation, no separate run step:
haxe --cwd tests/unit compile-macro.hxml

# Misc tests, optionally filtered to a single project (Issue1234):
haxe --cwd tests/misc -D MISC_TEST_FILTER=1234 compile.hxml
# Target-specific misc tests:
haxe --cwd tests/misc/{target} -D MISC_TEST_FILTER=1234 run.hxml
```

To run the full CI test suite for one or more targets, set the `TEST` environment variable (e.g. `TEST=js,macro`) and run `haxe RunCi.hxml` from `tests/`. See `tests/README.md`.

## Code Style and Conventions

### OCaml Code (Compiler)
- Follow existing code style in the codebase
- Use tabs for indentation (as seen in existing files)
- Avoid code duplication
- Prefer top-level functions over closures for functions with large bodies
- Comments are good, but keep the amount reasonable

### Haxe Code (Standard Library & Tests)
- Use tabs for indentation
- Follow Haxe naming conventions:
  - Classes: PascalCase
  - Functions/variables: camelCase
  - Constants: UPPER_CASE
- Target-specific code should use conditional compilation (`#if target`)

## Common Tasks

### Making Changes to the Compiler
1. Identify the relevant module in `src/`
2. Make minimal, focused changes
3. Build: `make haxe`
4. Add regression tests if fixing a bug
5. Execute the appropriate tests in one of the tests subdirectories
6. Even if everything works, check some output to ensure it looks good

### Making Changes to Standard Library
1. Modify files in `std/`
2. Consider cross-platform compatibility
3. Test on relevant targets
4. Update API documentation if needed

### Making Changes Related to Coroutines
Also run the appropriate target tests of hxcoro: https://github.com/HaxeFoundation/hxcoro/tree/master/tests

### Debugging
- Use `-D dump=pretty` to dump readable AST to `dump/` directory
- Check generated code for specific targets
- Most targets produce readable output for inspection

## Important Context

### Multi-Target Nature
- Changes must consider impact on all supported targets
- Target-specific behavior should be clearly documented
- Use conditional compilation appropriately

### Backwards Compatibility
- The compiler and standard library must maintain backwards compatibility
- Breaking changes require careful consideration and deprecation process

### Performance Considerations
- Avoid unnecessary allocations in hot compiler paths
- Standard library should have minimal overhead
- Optimization passes should not break correctness

## CI/CD
- CI runs on GitHub Actions (see `.github/workflows/`)
- Tests run on multiple platforms (Windows, Linux, macOS), on both x86_64 and ARM64
- All targets are tested in CI

### Investigating CI Failures

The steps below use the `gh` CLI. The checkout may have many remotes (contributors' forks); if `gh` does not resolve the upstream repository, pass `--repo HaxeFoundation/haxe`. If you have GitHub MCP tools instead, the equivalents are `pull_request_read`, `list_workflow_runs`, `get_workflow_run`, `list_workflow_jobs` and `get_job_logs` (with `failed_only: true`).

1. **Find the correct workflow run for the current PR commit**:
   - Get the PR's head commit: `gh pr view <pr> --json headRefOid,headRefName`
   - List recent runs for the branch: `gh run list --branch <branch>`
   - A run belongs to the right commit when its head SHA matches the PR's head commit
   - **Never assume a run's status from memory or a previous check - always query its live status** (`gh run view <run-id>`)

2. **Understand `status` vs `conclusion` - these are different fields**:
   - `status: in_progress` → the run is **actively running right now**
   - `status: completed` → the run is **finished**; check `conclusion` for the outcome
   - `conclusion: failure` → the run **failed** (it is NOT in progress, even if you previously saw it running)
   - `conclusion: success` → the run passed
   - `conclusion: cancelled` → the run was cancelled (e.g., a newer push superseded it)
   - **Do not describe a `completed` run as "in progress"**

3. Identify which jobs failed: `gh run view <run-id>` (look for jobs whose conclusion is not success)

4. Get the failing logs per job: `gh run view --job <job-id> --log-failed` (the job IDs are listed by `gh run view <run-id>`)
   - Avoid `gh run view <run-id> --log-failed` for whole runs: it downloads the logs of all jobs and is very slow
   - Logs end with post-step cleanup output; look at roughly the last 100-200 lines of the failing step rather than the very end
   - Look for keywords like `FAILED`, `error`, `exit code`, `assert`, `exception` to locate the relevant failure
   - Logs can be too large to read in one go: fetch them with a bounded tail (e.g. `tail_lines` for `get_job_logs`) or save them to a file, then search them instead of reading them in full

5. The job name contains the target and platform, e.g. `linux-test (macro, x86)` means the macro target on Linux x86

## Additional Resources
- Building instructions: `extra/BUILDING.md`
- Contributing guidelines: `CONTRIBUTING.md`
- Test suite overview: `tests/README.md`
- Unit test guidelines: `tests/unit/README.md`
- Misc test guidelines: `tests/misc/README.md`
- Main documentation: https://haxe.org/documentation/
- Manual: https://haxe.org/manual/

## Haxelib
- After building, `haxe` and `haxelib` executables are in the repository root; see "Use the Compiler You Just Built" above
- Run `haxelib setup` to set up the repository path
- Use `haxelib git utest https://github.com/haxe-utest/utest` for utest
- A git-cloned library can be set as a development library via `haxelib dev libname path`
- **hxcpp bootstrapping**: The git version of hxcpp requires bootstrapping before C++ compilation works. Run `cd $(haxelib libpath hxcpp)tools/hxcpp && haxe compile.hxml` after installing. Without this, `haxelib run hxcpp` will prompt for interactive input and hang. (The Copilot setup workflow, `.github/workflows/copilot-setup-steps.yml`, does this automatically.)
- C++ compilation is slow (minutes for a full build with all static libraries) but should produce constant output. If it hangs with no output, hxcpp likely needs bootstrapping.

## Other considerations
- If you feel like a given task makes little or no sense, point out why and suggest an alternative
- Keep an open mind regarding specific ways to address a given issue
