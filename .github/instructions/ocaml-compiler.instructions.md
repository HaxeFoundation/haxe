---
description: "Use when editing OCaml compiler sources under src/** (.ml/.mli), including typing, generators, macro, syntax, and optimization changes."
name: "Haxe OCaml Compiler Conventions"
applyTo: "src/**/*.{ml,mli}"
---
# Haxe OCaml Compiler Conventions

## Scope And Structure
- Keep changes narrow and localized to the relevant compiler subsystem (`typing/`, `generators/`, `macro/`, `syntax/`, `optimization/`, `filters/`, `context/`).
- Prefer extending existing module boundaries instead of introducing cross-cutting utility code.
- When possible, use top-level helper functions for larger logic blocks instead of deep closures.

## Style
- Follow existing file style exactly, including tab-based indentation where used.
- Match local naming and control-flow conventions of the touched module.
- Add short comments only where intent is not obvious from code structure.

## Correctness And Compatibility
- Treat changes as multi-target by default: verify impacts on all backends touched by the code path.
- Avoid behavior changes that break backward compatibility unless explicitly requested.
- Keep performance-sensitive paths simple and avoid unnecessary allocations in hot code.

## Build And Verification
- Build compiler changes with `make haxe` (or `opam exec -- make ADD_REVISION=1 -f Makefile.win -s -j haxe` on Windows setups using OPAM/Cygwin).
- If debugging typed/generator input, use `-D dump=pretty` and inspect the `dump/` output.
- Add or update a regression test when fixing a bug.
