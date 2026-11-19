# Agent Instructions for Haxe

This is the Haxe compiler repository: the compiler (OCaml, `src/`), the standard library (Haxe, `std/`) and the test suites (`tests/`).

This file only holds what applies to every task. Details live in the documents below; read the ones that match your task before starting, and don't read the others.

| When you... | Read |
| --- | --- |
| edit compiler sources (`src/**`) | `.github/instructions/ocaml-compiler.instructions.md` |
| add, change or run tests (`tests/**`) | `.github/instructions/tests.instructions.md` |
| need to find your way around `src/`, `std/` or `tests/` | `extra/agents/layout.md` |
| find the toolchain, `./haxe` or a haxelib library missing, or use haxelib or hxcpp | `extra/agents/setup.md` |
| investigate a CI failure | `extra/agents/ci.md` |
| chase an intermittent native crash in CI | `extra/debugging-native-crashes.md` |
| build on a machine that is not an agent container | `extra/BUILDING.md` |

## Building

```bash
export OPAMROOT=/opt/opam; eval "$(opam env)"   # needed in every new shell
make haxe                                       # builds the compiler and copies it to ./haxe
```

- `make haxe` is incremental: a no-op takes about a second, a full rebuild usually well under a minute. Run it again after changing compiler sources.
- Don't run a plain `dune build`: it uses dune's dev profile with different flags, so it rebuilds everything, and so does the next `make haxe`.
- On Windows: `opam exec -- make ADD_REVISION=1 -f Makefile.win -s -j haxe`
- Don't set up OCaml or opam by hand. If `dune`, `./haxe` or a haxelib library is missing, see `extra/agents/setup.md`.

## Use the Compiler You Just Built

The global PATH can point to a `haxe` binary from a different checkout or worktree. Call the compiler and haxelib from the repository root by path (`./haxe`, `./haxelib`; `.\haxe.exe` in PowerShell), or put the repository root first on PATH for the session. The `haxe` commands in these documents assume this has been taken care of.

## Rules

- Make minimal, focused changes and avoid code duplication.
- Use tabs for indentation, in OCaml and in Haxe, and follow the style of the surrounding code.
- When fixing a bug, add a regression test. Where it goes depends on what it tests (unit, misc, threads, sys, server, ...): see `.github/instructions/tests.instructions.md`.
- Even if everything works, check some output to ensure it looks good. Most targets produce readable code, and `-D dump=pretty` dumps the typed AST to `dump/`.
- Changes must consider all targets (JavaScript, C++, JVM, Lua, PHP, Python, HashLink, Neko, Flash and the interpreter). Target-specific code uses conditional compilation (`#if target`).
- The compiler and standard library must stay backwards compatible; breaking changes need a deprecation process.
- Avoid unnecessary allocations in hot compiler paths, and keep standard library overhead minimal.
- Readability is a cost too: every change has to be read and maintained by people. Weigh the added reading complexity against the benefit, and when the gain is small or uncertain, prefer the simpler version. The same applies when reviewing: don't suggest changes whose added complexity outweighs their gain.
- For changes related to coroutines, also run the appropriate target tests of hxcoro: https://github.com/HaxeFoundation/hxcoro/tree/master/tests
- `extra/CHANGES.txt` is assembled by the maintainers at release time; don't add entries to it unless asked.
- If a given task makes little or no sense, point out why and suggest an alternative. Keep an open mind regarding specific ways to address a given issue.

## Maintaining These Documents

Every line in this file is read at the start of every agent session, so it only earns its place if most tasks need it. Put anything else in the matching document from the table, or in a new one under `extra/agents/` with a row in the table that says when to read it. Reference documents by path; don't `@`-import them, which would load them on every session again.
