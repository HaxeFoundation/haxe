# Copilot Instructions for Haxe

## Repository Overview

This is the Haxe compiler repository. Haxe is an open source toolkit that allows building cross-platform tools and applications. The repository contains:

- **The Haxe compiler** - written in OCaml, targets multiple platforms (JavaScript, C++, JVM, Lua, PHP, Python, HashLink, NekoVM, Flash, and its own interpreter)
- **The Haxe standard library** - located in `std/`, written in Haxe
- **Test suite** - located in `tests/`, particularly unit tests in `tests/unit/`

## Build System

- **Language**: The compiler is written in OCaml (version 5.0+)
- **Build tool**: Uses `dune` build system and Make
- **Makefile**: For Unix/Linux/macOS, use `Makefile`; for Windows, use `Makefile.win`
- **Build commands**:
  - `make` - Build everything
  - `make haxe` - Build only the compiler
  - `opam install haxe --deps-only` - Install OCaml dependencies (after pinning the local checkout)
  - `opam exec -- make ADD_REVISION=1 -f Makefile.win -s -j haxe` - Typical Windows compiler build command used in CI/tasks
  - `dune build` - Build using dune directly

## Development Setup

### Prerequisites
- OCaml 5.0+ (managed via OPAM)
- Native libraries: PCRE2, zlib, mbedTLS 3.x (4.x not yet supported)
- Neko VM (for building haxelib)

## Code Organization

### Compiler Source (`src/`)
- `syntax/` - Lexer and parser
- `typing/` - Type system and type inference
- `codegen/` - Code generation utilities
- `generators/` - Target-specific code generators (JVM, JS, C++, etc.)
- `optimization/` - Optimization passes
- `macro/` - Macro system
- `context/` - Compiler context and state
- `filters/` - AST transformation filters

### Standard Library (`std/`)
- Core types (Array, String, Map, etc.) at root level
- `haxe/` and `sys/` - Platform-independent standard library packages
- Platform-specific directories (e.g., `cpp/`, `js/`, `jvm/`, `python/`, etc.)
  - Each platform can shadow standard library implementations via `platform/_std/` directory
  - Example: `std/cpp/_std/haxe/ds/StringMap.hx` shadows the generic `std/haxe/ds/StringMap.hx`

### Tests (`tests/`)
- `unit/` - Unit tests written in Haxe
  - `unit/src/unit/issues/` - Regression tests for specific issues (success cases)
- `server/` - Modern version of display tests, generally preferred
- `misc/` - Tests expected to produce failures
  - Platform-specific subdirectories (e.g., `misc/cpp/` for C++-specific tests)
- `nullsafety/` - Tests related to the null-safety feature
- `optimization/` - Optimization tests that check concrete code output on JavaScript target
- `sys/` - Tests specific to the `std/sys` package (sys-targets only: excludes Flash and JavaScript)
- `threads/` - Thread-related tests, generally for `std/sys/thread` package (threaded targets only)

## Testing Guidelines

### Adding Regression Tests

**For tests that assert success**, add a regression test to `tests/unit/src/unit/issues/`:

1. Create a file: `tests/unit/src/unit/issues/Issue{NUMBER}.hx`
2. Follow this pattern:
```haxe
package unit.issues;

class Issue12345 extends Test {
    #if target_name  // Use conditionals for target-specific tests
    function test() {
        eq(actualValue, expectedValue);
    }
    #end
}
```

**For expected failures**, use `tests/misc` instead, with platform-specific subdirectories as needed.

**Note**: Not all tests belong in the unit tests. See the Tests section above for other test directories and their purposes.

The Javascript tests are good for checking output syntax.

### Running Tests
```bash
# Compile for a specific target
haxe --cwd tests/unit compile-{target}.hxml

# Run the tests
# Example for Lua:
lua bin/unit.lua

# Example for JavaScript:
node bin/unit.js
```

## Code Style and Conventions

### OCaml Code (Compiler)
- Follow existing code style in the codebase
- Use tabs for indentation (as seen in existing files)
- Keep functions focused and modular
- Document complex algorithms or non-obvious logic

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
- The compiler should be fast and efficient
- Standard library should have minimal overhead
- Optimization passes should not break correctness

### Code Quality Considerations
- Try to avoid code duplication, especially in the OCaml code
- Prefer top-level functions over closures for functions with large bodies
- Comments are good, but keep the amount reasonable

## CI/CD
- CI runs on GitHub Actions (see `.github/workflows/`)
- Tests run on multiple platforms (Windows, Linux, macOS)
- Both x86_64 and ARM64 architectures are tested
- All targets are tested in CI
- When making changes related to coroutines, run the appropriate target test at https://github.com/HaxeFoundation/hxcoro/tree/master/tests

### Investigating CI Failures

When investigating CI failures, use the GitHub MCP tools as follows:

1. Use `list_workflow_runs` to find the relevant workflow run ID for the failing branch/PR
2. Use `list_workflow_jobs` to identify which jobs failed (look for `conclusion != "success"`)
3. Use `get_job_logs` with `failed_only: true` and `return_content: true` to get log content
   - **Important**: Use a moderate `tail_lines` value (100-200) to avoid being flooded by post-step cleanup output that always appears at the very end of logs
   - The actual test failure output typically appears near the bottom of the test step output, before the post-step cleanup lines
   - Look for keywords like `FAILED`, `error`, `exit code`, `assert`, `exception` to locate the relevant failure
4. The job name contains the target and platform, e.g. `linux-test (macro, x86)` means the macro target on Linux x86

## Additional Resources
- Building instructions: `extra/BUILDING.md`
- Contributing guidelines: `CONTRIBUTING.md`
- Unit test guidelines: `tests/unit/README.md`
- Main documentation: https://haxe.org/documentation/
- Manual: https://haxe.org/manual/

## Haxelib
- The haxelib command is available in your PATH (the workspace root is added to PATH during setup)
- Run `haxelib setup` to set up the repository path
- Use `haxelib git utest https://github.com/haxe-utest/utest` for utest
- A git-cloned library can be set as a development library via `haxelib dev libname path`
- If your changes are related to coroutines, run some of the tests in https://github.com/HaxeFoundation/hxcoro

## Other considerations
- Humans are fallible
- If you feel like a given task makes little or no sense, point out why and suggest an alternative
- Keep an open mind regarding specific ways to address a given issue
