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
  - `opam install . --deps-only` - Install OCaml dependencies
  - `dune build` - Build using dune directly

## Development Setup

### Prerequisites
- OCaml 5.0+ (managed via OPAM)
- Native libraries: PCRE2, zlib, mbedTLS 3.x (4.x not yet supported)
- Neko VM (for building haxelib)

### Getting Started
1. Clone with submodules: `git clone --recursive https://github.com/HaxeFoundation/haxe.git`
2. Install dependencies as documented in `extra/BUILDING.md`
3. Build the compiler: `make`

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
- `display/` - IDE-related tests like completion
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

### Running Tests
```bash
# Compile for a specific target
haxe --cwd tests/unit compile-{target}.hxml

# Run the tests
# Example for Lua:
lua bin/unit.lua
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
3. Build and test: `make && make test`
4. Add regression tests if fixing a bug

### Making Changes to Standard Library
1. Modify files in `std/`
2. Consider cross-platform compatibility
3. Test on relevant targets
4. Update API documentation if needed

### Debugging
- Use `-D dump=pretty` to dump AST to `dump/` directory
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

## CI/CD
- CI runs on GitHub Actions (see `.github/workflows/`)
- Tests run on multiple platforms (Windows, Linux, macOS)
- Both x86_64 and ARM64 architectures are tested
- All targets are tested in CI

## Additional Resources
- Building instructions: `extra/BUILDING.md`
- Contributing guidelines: `CONTRIBUTING.md`
- Unit test guidelines: `tests/unit/README.md`
- Main documentation: https://haxe.org/documentation/
- Manual: https://haxe.org/manual/

## Tips for Contributing

1. **Before filing issues**: Check if it's actually a compiler issue vs. a "how to" question
2. **Reduce test cases**: Create minimal examples without library dependencies
3. **Check development version**: Issue might already be fixed in development branch
4. **Target-specific issues**: Verify by checking generated code output
5. **Read CONTRIBUTING.md**: Contains important guidelines for issue reporting

## Version Compatibility
Be aware of version compatibility requirements between Haxe and target platforms (see compatibility table in README.md).
