# Haxe compiler

## Regression testing

The output of a compilation must not depend on the typing order, on the
number of domains typing in parallel, nor on whether it went through the
compilation server. A regression check covers all three, not only the
default configuration.

- **Test suites** (from `tests/`): `unit` (`compile-macro.hxml`,
  `compile-js.hxml`, `compile-hl.hxml`), `misc` (`compile.hxml`, then
  `run-base.hxml --run Main <target>`), `server` (`build.hxml` then
  `run.hxml`), `incremental` (`run.hxml`), `nullsafety`, `sourcemaps`,
  `threads`, `optimization`. Run `unit` again with `-D parallel-typing=0`.
- **Typing order**: compile with `-D typer-shuffle=<seed>` for several seeds
  (with `-D parallel-typing=0`, so that only the order changes) and compare the
  generated files with an unshuffled compilation.
- **Number of domains**: compile with `-D parallel-typing=<n>`, with few
  domains (`0`, `1`, `2`) and with many (`8`, `16`), and compare the outputs;
  several runs with the same setting must give the same output as well.
- **Warm/cold fuzzing**: `tests/fuzz/fuzz.py` applies random edits to a
  project and compares, after each edit, a compilation through the server with
  a compilation from scratch whose typing order and number of domains vary at
  random (the server itself uses a random number of domains, fixed for the
  run). Run it on large projects with macros (Heaps on JS and HL, hide), with
  several seeds:
  `python3 tests/fuzz/fuzz.py --dir <project> --out <output file> --src <dirs> --edits 50 --seed <n> --port <port>`.
  A `DIFF` line is a regression to investigate (the warm and cold compiler
  outputs are saved next to the project).
- **Errors**: part of the fuzzing edits (`--errors`, a probability, 0.3 by
  default) introduce an error at a given level: syntax, unknown type in a
  signature, field initialization, method body, unknown identifier, argument
  of a macro, error raised by a macro. The warm and cold compilations must
  report the same errors, and the server must give the same output as a cold
  compilation once the error is undone.
- **Memory**: with `--mem`, the fuzzer measures what the server keeps alive
  after each compilation (`server/gcCompact`, field `live`; `after` is the
  size of the heap, free space included) and reports a `LEAK` line when it
  grows by more than `--leak-mb` between the first successful compilations and
  the last ones. Failed compilations must not stay alive either: run it with
  errors and with several domains, since the state of the domains of the pool
  outlives a compilation.

Pitfalls:
- Do not run two instances of the incremental runner at once: it deletes its
  shared work directory (`tests/incremental/work`) when it starts.
- Do not rebuild the compiler while a long test or fuzzing run uses it: a
  server started before the rebuild is compared with compilations of the new
  binary.
- A server that dies during a compilation gives the client an empty output and
  exit code 0; the incremental runner reports whether the server still answers
  and what it printed.
- Some `misc` tests install libraries in the haxelib repository: check that
  the libraries a later check needs are still registered.
