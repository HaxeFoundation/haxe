# Repository Layout

## Compiler Source (`src/`)
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

## Compiler Definitions (`src-json/`)
- `define.json` - Compiler defines (`-D` flags)
- `meta.json` - Metadata (`@:meta`)
- `warning.json` - Warnings
- Add new defines, metadata and warnings here; the corresponding OCaml code is generated at build time

## Standard Library (`std/`)
- Core types (Array, String, Map, etc.) at root level
- `haxe/` and `sys/` - Platform-independent standard library packages
- Platform-specific directories (e.g., `cpp/`, `js/`, `jvm/`, `python/`, etc.)
  - Each platform can shadow standard library implementations via `platform/_std/` directory
  - Example: `std/cpp/_std/haxe/ds/StringMap.hx` shadows the generic `std/haxe/ds/StringMap.hx`
- Haxe naming conventions: classes PascalCase, functions and variables camelCase, constants UPPER_CASE
- When changing the standard library, test on the relevant targets and update the API documentation if needed

## Tests (`tests/`)
- `unit/` - Unit tests written in Haxe
  - `unit/src/unit/issues/` - Regression tests for specific issues (success cases)
  - `unit/src/unit/hxcpp_issues/` - Regression tests for issues from the hxcpp repository
- `misc/` - Project-based compiler tests, mainly for checking compiler errors and warnings
  - `misc/eval/projects/` - Target-independent projects (run on the eval target)
  - `misc/{target}/projects/` (e.g., `misc/python/projects/`, `misc/hl/projects/`) for target-specific projects
- `server/` - Compilation server and display/completion tests
- `nullsafety/` - Tests related to the null-safety feature
- `optimization/` - Optimization tests that check concrete code output on JavaScript target
- `sys/` - Tests specific to the `std/sys` package (sys-targets only: excludes Flash and JavaScript)
- `threads/` - Thread-related tests, generally for `std/sys/thread` package (threaded targets only)
- `RunCi.hx` and `runci/` - The script CI uses to run all test suites; `runci/targets/*.hx` has the exact commands CI runs for each target and suite

## Other Directories
- `libs/` - Bundled OCaml libraries the compiler links against (extc, swflib, ziplib, the mbedtls and neko bindings, ...), built by dune together with `src/`
- `plugins/` - Compiler plugins (`make plugin PLUGIN=example`)
- `extra/` - Build, release and setup helpers
- `.github/workflows/` - CI (GitHub Actions): all targets, on Windows, Linux and macOS, on both x86_64 and ARM64
