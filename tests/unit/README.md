# Unit Tests

## Adding Regression Tests for Bug Fixes

When fixing a bug, add a regression test in `src/unit/issues/`:

```
tests/unit/src/unit/issues/Issue12345.hx
```

This pattern:
- Avoids merge conflicts (each issue gets its own file)
- Makes it clear which issue each test covers
- Automatically runs with the test suite

### Example

```haxe
package unit.issues;

class Issue12345 extends Test {
    // Use #if for target-specific tests
    #if lua
    function test() {
        // Test that would fail without the fix
        eq(actualValue, expectedValue);
    }
    #end
}
```

## Test File Locations

| Location | Purpose |
|----------|---------|
| `src/unit/issues/IssueXXXXX.hx` | Per-issue regression tests |
| `src/unit/Test*.hx` | General feature/target tests |
| `src/unit/spec/` | Specification tests |

## Running Tests

```bash
# Compile for a target (e.g., Lua)
haxe --cwd tests/unit compile-lua.hxml

# Run
lua bin/unit.lua
```

See `compile-*.hxml` files for other targets.
