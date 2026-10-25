# Faster macro execution (eval)

Plan to speed up the execution of macros (and `--interp`), without depending
on any third-party tool: no LLVM or other code generator, no OCaml toolchain
at run time, no new library.

## 1. Where macro time goes

Macros run on the eval interpreter (`src/macro/eval`). Its "JIT"
(`evalJit.ml`) compiles the typed AST of a function to a tree of OCaml
closures (`evalEmitter.ml`), once per function: compiling is cheap (hide:
12 202 functions, 0.08 s); executing is what costs.

hide, main compilation on one domain (`--times -D macro-times
-D parallel-typing=0`): macro execution 2.17 s out of 10.2 s, of which
`hxsl.Macros.buildShader` 1.30 s (it type checks and links shaders with the
hxsl compiler, interpreted), `hrt.prefab.EditContext2.build` 0.36 s,
`hrt.prefab.Macros.getSerializableProps` 0.12 s. A large part of it is
interpreted code, not calls of the compiler API.

Micro benchmarks of the code macros run (`--interp`, same engine as macros,
against the same program compiled to JS and run by node; measured on a loaded
machine, to measure again in step 0):

| Benchmark | eval | node | ratio |
|---|---|---|---|
| calls and integer arithmetic (`fib`) | 75.6 s | 3.4 s | 22× |
| objects (`new`, methods, fields) | 587 ms | 18 ms | 32× |
| arrays | 156 ms | 4 ms | 39× |
| anonymous structures, `Dynamic` | 169 ms | 6 ms | 28× |
| closures | 408 ms | 18 ms | 22× |
| string building | 189 ms | 112 ms | 1.7× |
| maps | 108 ms | 31 ms | 3.5× |

Strings and maps are close because their work is done by OCaml code of the
eval standard library. Calls, arithmetic, field and array accesses are where
the interpreter itself is slow.

## 2. What makes them slow

- **Calls.** A call evaluates its arguments into a `value list`
  (`List.map`), then the callee:
  - allocates its environment, a record of 11 fields, plus an array of
    locals and one of captures (`EvalContext.push_environment`);
  - reads the evaluation state of the domain (`get_eval`) and checks the
    debugger and timer settings at run time;
  - runs its body under `Std.finally` (a closure allocated by call);
  - matches the argument list against the parameter setters
    (`process_arguments`);
  - catches the `Return` exception when the function returns before its end.
- **Boxed integers.** `Int` is `VInt32 of int32`: an OCaml `int32` is a
  boxed custom block, so every integer result (`i + 1`, an array index, a
  loop counter) allocates. `Float` (`VFloat of float`) is boxed as well.
- **Generic operators.** `a + b` matches the pair of values against every
  combination (`EvalMisc.op_add`: Int32, Int64, Float, strings…) even when
  the typed AST tells that both are `Int`. Comparisons and equality are the
  same.
- **Dynamic dispatch.** A call of a method that is not final looks the
  method up in the prototype by hashed name (`IntMap`) at every call
  (`emit_method_call`); in macro mode no method can be assumed final, since an
  override may be added later. Reading a field of an anonymous structure or a
  `Dynamic` value looks the name up as well.
- **Allocation.** Environments, argument lists, boxed numbers and closures
  for `finally` make the minor heap fill quickly; with several domains
  running macros, minor collections stop every domain.

## 3. Options

- **A native JIT** (generating x86-64 and ARM64 code in OCaml): the code would
  have to allocate in the OCaml heap, respect the write barrier, describe its
  roots to the GC and support exceptions and the eval debugger, on two
  architectures. Too large and too risky. Rejected.
- **Compiling macros to OCaml** and loading them with `Dynlink`: needs an
  OCaml compiler at run time. Rejected (third-party tool).
- **Running macros on HashLink and its JIT**: HashLink is a separate runtime;
  every value crossing the compiler API (expressions, types) would have to be
  converted both ways, and macros share compiler state. Rejected for now.
- **Making the closure compiler faster** (chosen): closure compilation can
  get within a small factor of a baseline JIT when the calling convention,
  the value representation and static types are used well. Everything stays
  in OCaml, works on every platform, and keeps the debugger and the parallel
  execution of macros.

## 4. Plan

Each step is measured on the benchmarks and on Heaps and hide, and keeps
their output identical (see `CLAUDE.md`, regression testing).

0. **Measurement.** A benchmark suite in `tests/benchs/eval`: the
   micro benchmarks above (calls, objects, arrays, anonymous structures,
   closures, strings, maps, enums and `switch`) and an hxsl-like workload,
   run both with `--interp` and as macros; plus macro execution times of
   Heaps and hide (`-D macro-times`), on an idle machine. Kept as the
   reference for the following steps.
1. **Calling convention.**
   - Functions take their arguments in an array, or as fixed-arity OCaml
     closures for 0 to 4 arguments, instead of a list.
   - The environment is reduced to what execution needs: the debugger and
     timer data are only allocated when they are enabled, decided when
     jitting rather than at every call.
   - The domain's evaluation state is passed along instead of being looked up.
   - `Std.finally` is replaced by a direct handler.
   - Expected: calls 2 to 3× faster (the `fib` and objects benchmarks).
2. **Control flow without exceptions.** A `return` before the end of a
   function, `break` and `continue` are compiled as values returned by the
   statement closures (a small control code), keeping exceptions only for
   `throw`. Measured first: OCaml exceptions are cheap, the gain may be small.
3. **Unboxed integers.** `VInt32 of int32` becomes `VInt32 of int`, holding
   the value normalized to 32 bits (`EvalValue.wrap32`), wrapping applied
   where an operation can overflow. A constructor with an argument is still
   a block in OCaml: an Int is one allocation of 2 words instead of two (the
   block and the custom block of the `int32`), and the operations are on
   native integers. Done: on hide, macro execution −4 %; benchmarks arrays
   −12 %, closures and maps −6 %, objects −4 %, anon and shader within the
   noise (+1 to +7 %).
4. **Static specialization.** The jit uses the types of the typed AST:
   - operators on `Int`, `Float` and `Bool` operands get their own emitters,
     without matching every case;
   - comparisons and equality likewise;
   - `a[i]` on an `Array<T>` with an `Int` index;
   - `for (i in a...b)` as a native OCaml loop;
   - `for (x in array)` over the array without an iterator object.
5. **Inline caches.** A call of a method that is not final caches, at the
   call site, the prototype it last saw and the method found (a monomorphic
   cache, checked by physical equality of the prototype). A field read on an
   anonymous structure caches its shape and slot. The caches are written
   without a lock: a race only costs a cache miss, as long as an entry (a
   prototype and its function) is published as one immutable value.
6. **Allocation.** Captured variables, closures and temporary arrays are
   reviewed with the allocation profile of the benchmarks. The minor heap
   size of the domains running macros is tuned.
7. **Super-instructions** (only if the previous steps leave a clear
   hotspot): fused emitters for frequent patterns (a field of `this`, a
   local compared with a constant, an increment of a local).

Expected overall: the interpreter-bound benchmarks from 20–40× slower than
node to 5–10×, and macro execution on hide from about 2.2 s to about 1 s on
one domain.

## 5. Constraints and verification

- **Semantics.**
  - `Int` keeps 32-bit wrapping semantics, and `haxe.Int64` stays boxed.
  - Evaluation order, exceptions and positions in error stacks are unchanged.
  - The eval debugger keeps its view of locals and captures, through the
    debug data allocated when it is enabled.
- **Parallel macros.** The new caches and specialized closures must be safe
  when several domains run macros (immutable entries, no lock on the fast
  path).
- **Tests.**
  - `tests/unit` with `compile-macro.hxml` (macros) and `--interp`;
  - `tests/misc` (eval);
  - `tests/server` and `tests/incremental`;
  - the output of Heaps and hide, identical;
  - the warm/cold fuzzing (`tests/fuzz/fuzz.py`).
