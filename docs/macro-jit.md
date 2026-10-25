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
3. **Unboxed integers.** `VInt32 of int32` becomes `VInt of int` holding
   the value normalized to 32 bits (an immediate, never allocated); `Int32`
   wrapping is applied where an operation can overflow. The helpers
   (`vint`, `decode_int`, the encoders) keep the change mechanical across the
   eval standard library. Expected: integer arithmetic, loops and array
   accesses no longer allocate.
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

## 5. Status (implemented)

Measured on a 4-core cloud VM (noisy: medians of alternating runs), Heaps
`haxe6-annotations` with hxsl, domkit, hxbit and castle: `-D apicheck` build
to JS on one domain (`-D parallel-typing=0 --times -D times.macro`), and its
43 samples compiled one by one (macro execution summed). Generated code is
identical to the compiler before these changes for all 44 outputs.

| | before | after |
|---|---|---|
| Heaps apicheck, macro execution (median of 9) | 0.57 s | 0.39 s |
| Heaps apicheck, all macro time incl. typing and jit (median of 9) | 1.35 s | 1.08 s |
| Heaps samples, macro execution summed (2 runs each) | 8.28 s / 7.55 s | 5.63 s / 6.29 s |
| micro benchmarks, `--interp` total (`tests/benchs/eval`) | 2.97 s | 2.1 s |

What was done, by plan step:

0. `tests/benchs/eval`: calls, loops, floats, objects, arrays, anonymous
   structures, closures, strings, maps and an hxsl-like workload (build,
   type check and evaluate enum trees), run with `interp.hxml` or
   `macro.hxml`; each case prints a checksum.
1. Calling convention, partly: the emitters return their closure
   (`fun env ->`) instead of being applied partially (no `caml_curry`
   between nodes); arguments are evaluated by a closure made for their
   number and stored by slot (no closure per parameter, none allocated per
   call); `Std.finally` is replaced by a direct handler; `get_eval` no longer
   allocates; small arrays of locals are allocated inline; the stack limit is
   read when jitting; static, constructor and super calls keep their forced
   target at the call site. Arguments are still a list and the evaluation
   state is still looked up per call: on Heaps that lookup is about 1% of
   macro time, which did not justify changing `vfunc`.
2. Not done: exceptions for `return`/`break` are cheap in OCaml and do not
   show in the profiles.
3. Done: `VInt of int`, wrapped to 32 bits (`wrap32`). Neutral on time
   (OCaml allocates a boxed int32 inline), fewer words allocated.
4. Done dynamically rather than from the types: comparisons and arithmetic
   check for two `Int` or two `Float` values in the emitter before the
   generic operator. Conditions (`if`, `while`, `&&`, `||`, `!`, comparisons,
   `== null`) are compiled to closures giving an OCaml bool. `for` loops are
   already lowered to `while` by the typer and are not specialized.
5. Done for method calls (monomorphic cache on the prototype). Field reads of
   anonymous structures already compare the prototype computed when jitting.
   The macro API's `encode_obj` caches, per domain, the prototype and slots
   for a list of field names, instead of sorting them and taking a lock per
   encoded object; field names given by the API are hashed through a cache by
   string identity, and `EvalHash.hash` checks a direct-mapped cache before
   its table.
6. Closures allocated per execution were removed from calls, `switch` and
   `try`.
7. Operand variants: comparisons, arithmetic, `local op= e`, field, array,
   `length`, enum index and parameter reads, null checks and field writes
   read a local (or `this`) or a constant directly instead of calling a
   closure; `this.f()` has no null check.
8. Added: the methods of the macro context are jitted on their first call
   (3367 functions were jitted eagerly when their prototypes were built for
   the Heaps build, most never called).

Where the rest of the time goes (Heaps, eval part of the profile): the walk
of the closure tree (about a third), runtime helpers (field lookups by name,
calls, `StringMap`), the jit, and compiler work done by API calls
(`Context.typeof`...), which is not interpretation. The next steps would be
the calling convention (arguments as an array, the evaluation state passed
along) and fused emitters for whole statements.

## 6. Next phase: where macro time goes now

Heaps `-D apicheck` with parallel typing (the default configuration): all
macro time 0.89 s -> 0.61 s, macro execution 0.28 s -> 0.20 s (medians of
7). Execution is now a third of macro time; the rest is loading the macro
modules (parsing and typing hxsl, domkit, hxbit... in the macro context, its
filters and its flush into the interpreter), compiler work outside eval.

In the eval part of the profile (one domain): the closure tree 41%, the
standard library 15% (StringMap, strings, arrays), calls 6%, the macro API
5%, field lookups 5%, GC 4%, jit 2%. Parsing seen during execution is
`Context.getType` loading modules of the main context (typing, not eval).

Steps, by expected gain over effort:

1. **Fewer nodes for frequent shapes.**
   - `return e` in tail position is `e` itself, not a wrapper node.
   - A `switch` on an enum (`TSwitch` on `TEnumIndex`) reads the index of
     the value directly, without building an `Int`; with a local subject, without
     calling a closure.
   - Call arguments that are locals or constants are read directly.
   - `var x = e` with a local or constant `e`.
   Done: the tail `return`, the enum switches, call and enum-constructor
   arguments, array and object literals and `var x = e` take locals and
   constants as operands. About 4% on Heaps macro execution (0.542 s ->
   0.519 s, median of 9, within the noise of this machine), 1.02x on the
   micro benchmarks.
2. **Standard library.** `StringMap` keeps a sorted map (its iteration order is
   observable and must not change). Not done: map operations are 1.7% of eval
   time on Heaps and most maps are small, where a hash table (plus sorting
   the keys when iterating, and an O(n) `copy`) is not cheaper than the
   balanced tree.
3. **Calling convention.** Arguments in an array and the evaluation state
   passed along: about 6% of eval time on Heaps, a change of `vfunc` across
   the standard library. Only if 1 and 2 leave calls as the next hotspot.
4. **Loading the macro modules.** Now the larger part of macro time on a
   cold build, and the critical path: on Heaps with parallel typing, the claim
   of the macros is held 0.82 s in the `types` stage, 0.805 s of it by 9
   macro loads (the first call of a macro loads its module and what it
   imports into the macro context), while the other domains wait 0.71 s for
   the macros and 0.94 s for classes being built. A load, on one domain:
   typing the bodies (`Finalization.finalize` of the macro context) 77%,
   creating the modules 25% (parsing 21%), the flush into the interpreter 14%
   (the filters of the new types 9.5%, the prototypes 4.5%), ordering the
   types 3.5%. The macro context types its bodies on one domain (its
   `run_parallel` is `None`) and parallel batches do not nest (one global
   batch, `Claims.start_tasks`). Parsing is already shared with the main
   context when the defines read agree: of the 380 files the macro context
   needs, 161 come from the main context; 79 are parsed by both because they
   read `macro`.

   Options:
   - *Parallel typing of the macro context's bodies during a load*, by the
     domains waiting for it: same results, but a second kind of parallel
     batch, nested in the global one, with its own task distribution and the
     same ordering of the units as on one domain. **Chosen, implemented:**
     - The macro context has a `run_parallel` of its own
       (`TyperEntry.run_macro_tasks`) and `nested_parallel`: its parallel
       units (grouped by module, each task with its own queues, merged in task
       order afterwards, as for the main context) start a batch from the
       domain loading a macro, outside of the units of the macro context and
       of the tasks of its batches, while the main context types in parallel
       or not. Outside of its tasks the macro context runs its stages with
       the claim of the macros only, as before; inside them, with the typing
       lock like the tasks of the main context.
     - The tasks are taken in order by the domain loading the macro, by the
       domains of the pool with nothing to do (`Task.async`, never awaited:
       a continuation resumed by the pool can go on on another domain, whose
       domain-local state is not its own) and by the domains waiting for the
       claim of the macros (`Claims.wait` runs a task instead of sleeping;
       the domain is not registered as waiting meanwhile, or the cycle
       detection sees false cycles and lets a domain take the claim).
     - The claim of the macros held by the loading domain is lent to the
       batch (`Claims.lend_claim`): a task that runs a macro takes it over
       while the lent claim is on top, one task at a time, so that macros
       still run on one domain at a time. A domain running a task has the
       speculation state of a domain holding no claim
       (`Claims.as_batch_task`), restored afterwards.
     - Not in display mode (the main context does not type in parallel
       then), nor with `-D parallel-typing=0/1`.
     - Measured on Heaps `-D apicheck` (4-core machine, so no domain of the
       pool is idle while the main context types in parallel; median of 7):
       the claim of the macros held in the `types` stage 0.79 s -> 0.61 s,
       the domains waiting for the macros 0.69 s -> 0.51 s, for classes
       being built 0.86 s -> 0.68 s, all macro time 0.73 s -> 0.64 s, the
       whole compilation 4.55 s -> 4.40 s. With the main context typing on
       one domain (pool idle), the batches of the macro context take 0.70 s
       -> 0.48 s. On a machine with more hardware threads than typing
       domains (22 against 4 on SpaceCraft), the idle domains of the pool
       take tasks too.
     - Checked: Heaps output identical with `-D parallel-typing=0,1,2,3,8,16`
       (two runs each) and for the 43 samples; `tests/unit` (macro, also with
       `-D parallel-typing=0`, JS), `tests/misc`, `tests/server`,
       `tests/incremental`, `tests/threads` pass. Warm/cold fuzzing on Heaps
       (seed 1): one difference, after an error that makes a build macro
       fail, which is not from this change: the compiler before it reports
       the same messages, and already reports more errors in cascade on one
       domain (83, the shaders whose build fails) than on four (35), so the
       warm compilation (server on one domain) and the cold one differ.
       **Fixed (next commit):** an error reported in the macro context
       disabled the interpreter (`had_error`): every macro called after it
       failed (Build failure, then the errors of the class body typed
       without its build), and which macros came after depended on the
       order of typing (2592 against 1920 errors for two `-D typer-shuffle`
       seeds on one domain). Now the macros still run; a macro whose own
       function reported an error does not run (its body is typed before it
       is called, whatever the order), one calling what failed to type fails
       when it calls it. The same error in Heaps now gives the two messages
       of the error itself, whatever the order and the number of domains.
       Warm/cold fuzzing on Heaps after the fix: seed 5 (server on 8
       domains, 25 edits, the two differences it found before) and seed 1
       (server on 1 domain, 30 edits): no difference.
   - *Lazy bodies in the macro context*: a body typed on the first call of its
     function (methods are already jitted then), the others typed at the end
     of the compilation so that their errors are still reported (later than
     today). Removes most of the load from the critical path; changes when
     errors in unused macro code appear, and needs the typing of a body from
     a macro running without the claim.
   - *Loading the macro modules earlier*: start the load of the macro named
     by `@:build`/`@:autoBuild` metadata as soon as the module declaring it is
     parsed, on a domain with nothing else to do. Same work, off the critical
     path when there is idle time; the macro context then loads its modules in
     another order (which must not change the output).

## 7. Constraints and verification

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
