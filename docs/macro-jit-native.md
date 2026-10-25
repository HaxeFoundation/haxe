# Native JIT for macros (eval), on the OCaml backend

Alternative to `macro-jit.md`: instead of making the closure compiler faster,
compile hot macro functions to machine code in process, reusing the native
backend of the OCaml compiler. Nothing is installed besides what already
builds Haxe: the backend is the source code of the OCaml compiler, vendored in
`libs/jit/ocaml` (section 3.2), and no assembler, linker, C compiler or LLVM is
needed at run time.

`macro-jit.md` rejects a native JIT because the code "would have to allocate
in the OCaml heap, respect the write barrier, describe its roots to the GC and
support exceptions, on two architectures". The OCaml backend already does all
of that: it is what `ocamlopt` uses. What it lacks is the last mile, from
assembly to executable memory, and that is the part this plan writes.

## 1. What OCaml provides

Checked on the OCaml 5.3 sources (`asmcomp/`, `runtime/`):

| Need | Provided by |
|---|---|
| IR with GC-safe allocation, write barrier, exceptions, polling, stack growth | `Cmm` (`Cfunction`, `Cdata`), `Cmm_helpers` (boxing, `caml_modify`, static closures) |
| Cmm → machine code: selection, register allocation, frame tables | `Asmgen.compile_phrase : ppf_dump:_ -> Cmm.phrase -> unit` |
| amd64 output as data, not text | `X86_proc.register_internal_assembler`: receives the `X86_ast` instruction list (the hook used by LexiFi's binary emitter), called from `Emit.end_assembly` |
| arm64 output | text only (`arm64/emit.mlp` prints GNU `as` syntax) |
| registering code with the runtime | `caml_register_frametables`, `caml_register_dyn_globals`, `caml_register_code_fragment` (what `dynlink_nat.c` calls for a native plugin) |
| generic application functions | `Cmm_helpers.generic_functions` (`caml_applyN`, `caml_curryN`, as the linker generates them) |

The vendored backend is the one of the compiler that builds Haxe (same
version, configuration generated from `ocamlopt -config`, domain state layout
from the installed runtime headers), for the host architecture: the generated
code matches the runtime it runs in (domain state layout, frame table format,
closure info).

What is missing, and is the work of this plan:
1. a translator from the typed AST of a macro function to Cmm;
2. a binary encoder for amd64 (`X86_ast` → bytes and relocations) and an
   assembler for the subset of arm64 assembly that `emit.mlp` prints;
3. an in-memory linker and loader (executable memory, relocations, symbols,
   registration with the runtime);
4. the integration with eval: compilation ahead of time, interoperation with
   interpreted code,
   lifetime in the compilation server.

## 2. Pipeline

```
texpr (TFunction)
  └─ evalNative.ml ─────────────► Cmm phrases (one unit per batch of functions)
       Asmgen.compile_phrase      Cmm → Mach → Linear → Emit
         amd64: X86_ast list ───► evalNativeX86.ml: bytes + relocations
         arm64: assembly text ──► evalNativeArm64.ml: bytes + relocations
  └─ evalNativeLink.ml + C stubs: executable memory, GOT/PLT, symbols,
       frame tables, GC roots, code fragment
  └─ static closure of the function ──► VFunction (vfunc = value list -> value)
```

A natively compiled function is an ordinary OCaml closure of type `vfunc`:
eval calls it like any other function, and it calls interpreted functions,
helpers of the eval runtime and the compiler API through OCaml closures. The
two tiers mix at function granularity.

## 3. Components

### 3.1 Translator (`evalNative.ml`)

- Input: the same typed function as `EvalJit.jit_tfunction`. Output: one
  `Cfunction` taking the argument list (later, fixed arities) and returning a
  `value`, and its static closure (`Cdata`, uncollectable header, as
  `ocamlopt` emits constant closures).
- First version: every Haxe value is an eval `value` (machtype `Val`); every
  operation calls the existing OCaml implementation (`EvalMisc.op_add`, field
  access, prototype lookup…) through a **helper table**: an OCaml block of
  closures, stored in a data slot of the unit and registered as a GC root,
  filled by the loader. No OCaml symbol of the Haxe binary is referenced by
  name (their names, `camlEvalMisc__op_add_1234`, are not stable).
- Constructors of `EvalValue.value` (`VInt32`, `VFloat`, `VObject`…) are
  built and matched by tag. The tags are generated from the type definition
  and checked at startup against `Obj.tag` of sample values: a change of
  `evalValue.ml` that the translator does not follow disables the native tier
  instead of corrupting the heap.
- Unsupported constructs (first version: the debugger, `untyped` code with
  special semantics, whatever is not done yet) leave the whole function in the
  closure tier.
- Eval semantics kept: evaluation order, 32-bit `Int` wrapping, exceptions as
  the same OCaml exceptions (`RunTimeException`, passed in the helper table),
  positions for error stacks (the position is stored in the frame before each
  call or throw, as the closure tier does).

### 3.2 Vendoring and driving the backend (`libs/jit`)

- `compiler-libs` cannot be linked into Haxe: several of its modules have the
  same names as modules of the Haxe compiler (`Path`, `Lexer`, `Parser`,
  `Typecore`…), and `Asmgen` pulls the whole frontend through `Translmod`
  (checked in step 0: "inconsistent assumptions over interface Path"). The
  backend is vendored instead, as the wrapped dune library `ocaml_backend`,
  by `libs/jit/ocaml/vendor.py` from an OCaml source tree of exactly the
  version that builds Haxe: the closure (ocamldep) of the modules needed from
  Cmm to assembly, without `Cmmgen` and `Translmod` (their entry points are
  removed from `Asmgen`). `Config` is generated at build time from
  `ocamlopt -config`, `Domainstate` from the installed runtime headers.
  The libraries are enabled only for that OCaml version; with another one,
  `libs/jit` is skipped and Haxe builds without the native tier.
- The backend keeps global state (`Clflags`, `Compilenv`, `Reg` counters,
  `Emitaux`, `X86_proc.asm_code`): one compilation at a time, under a lock,
  ideally on a dedicated domain so that macro execution does not wait.
- One compilation unit per batch, with a unique name (`camlHxJit_<n>`):
  `Compilenv.reset`, `Emit.begin_assembly`, `compile_phrase` for each function,
  the generic functions it needs, `Emit.end_assembly`.
- Settings: `Clflags.dlcode` and `pic_code` (external symbols go through a
  GOT/PLT, so code can live anywhere in the address space), linear scan
  register allocation (`use_linscan`, faster to compile), `-O`-level inlining
  of the Cmm itself is left to the translator. Text output goes to the null
  device on amd64.

### 3.3 Encoders

- **amd64**: `X86_ast` → bytes, for the instructions and directives that
  `amd64/emit.mlp` produces (a closed set, about 80 instruction forms, plus
  labels, sections, `.quad`/`.long`/`.byte`, alignment), with the usual branch
  relaxation (short/long jumps). Relocations kept: PC-relative 32-bit,
  absolute 64-bit, GOT/PLT references.
- **arm64**: `emit.mlp` writes text; the assembler parses the subset it
  prints (fixed-width instructions, `adrp`/`add :lo12:`, `:got:`, literal
  pools, the same directives). Alternative: a vendored copy of `emit.mlp`
  writing a structured instruction type, which avoids parsing but must be
  updated with each OCaml version. The text assembler is preferred: no OCaml
  source is patched.
- **Verification in CI only** (where the tools exist): for every unit jitted
  by the test suites, compare the bytes with what GNU `as` / `clang` assemble
  from the same unit printed as text. Any difference is a bug of the encoder.

### 3.4 Linker and loader (`libs/jit/jit_link.ml`, `libs/jit/jit_stubs.c`)

- Executable memory per arena: `mmap` + `mprotect` (W^X: written, then made
  executable) on Linux; `MAP_JIT` with `pthread_jit_write_protect_np` and
  `sys_icache_invalidate` on macOS (the signed, hardened Haxe binary needs the
  `com.apple.security.cs.allow-jit` entitlement); `VirtualAlloc` /
  `VirtualProtect` / `FlushInstructionCache` on Windows. Instruction cache
  flush on arm64.
- Symbols: first the unit's own symbols, then a **static table in C** of the
  runtime symbols the backend references (`caml_call_gc`, `caml_allocN`,
  `caml_c_call`, `caml_raise_exn`, `caml_modify`, `caml_initialize`,
  `caml_call_realloc_stack`, `caml_ml_array_bound_error`, `caml_int32_ops`,
  `caml_exn_*`…). No `dlsym`, so the Haxe executable does not need to export
  its symbols. An unknown symbol fails the batch, which stays interpreted.
- GOT and PLT stubs per arena (absolute addresses), so the arena can be far
  from the runtime (beyond ±2 GB on amd64, ±128 MB for `bl` on arm64).
- Registration, as `caml_natdynlink_register` does: frame tables, GC roots
  (the helper table slot), code fragment. Frame table registration is a
  stop-the-world on all domains (`caml_try_run_on_all_domains`): it is done
  once per batch, never per function.

### 3.5 Integration with eval

- **Ahead of time, no tiering.** Functions are compiled to native code where
  eval compiles them to closures today (`EvalJit`, when a type is loaded into
  the macro context), one OCaml unit per batch of functions (the types loaded
  together), so that the runtime registration (a stop-the-world) happens once
  per batch. A function the translator does not support is compiled to
  closures, as today. There is no counter, no background compilation and no
  swap of installed code: simpler, and the generated code never changes
  under a running macro.
- **Disk cache** (later): the units of a batch, keyed by the digest of the
  typed functions and the backend version, so that a cold compilation does
  not compile again the macros of the libraries that did not change.
- **Defines.** `-D eval-native=0|1`: off, or on (default once stable).
  `-D macro-times` reports native compile time separately.
- **Debugger.** With the eval debugger enabled the native tier is off.
- **Compilation server.** Code is keyed like the closure tier's cache. Arenas
  belong to the macro context; when it is discarded (a module of macro code
  changed), its frame tables are unregistered (`caml_unregister_frametables`)
  and its memory freed, only once no domain runs macros, since a live closure
  or stack frame may still point into it. A version stamp of the backend
  (OCaml version, Haxe build) is part of the key.

## 4. Plan

Each step keeps the outputs of Heaps and hide identical and is measured as in
`macro-jit.md` (same step 0).

0. **Measurement**, shared with `macro-jit.md` step 0, and a **spike**
   (done, see section 7):
   a hand-written Cmm `fib` on tagged ints, compiled through
   `compiler-libs.optcomp` inside Haxe on Linux amd64, encoded, loaded and
   called. Exit criterion: correct results under a GC stress run (small minor
   heap, forced major collections) with several domains calling it.
   Also measured: backend compile time per function, binary size increase.
1. **amd64 encoder** for the full output of `amd64/emit.mlp`, with the CI
   comparison against `as`.
2. **Linker and loader** on Linux, then macOS (x86-64) and Windows (mingw64);
   arena lifetime and unregistration.
3. **Translator, value level.** Every value boxed as an eval `value`, every
   operation through helpers. Goal: correctness — `tests/unit` with
   `compile-macro.hxml` and `--interp` pass with `-D eval-native=1`.
   The gain here comes only from removing the closure tree (dispatch per node,
   environment handling).
4. **Ahead-of-time compilation in eval**: batches per loaded types, fallback
   to closures, server lifetime, `-D macro-times`; then the disk cache.
5. **Typed specialization**, where the native tier earns its keep:
   - `Int`, `Float` and `Bool` locals unboxed in registers (untagged ints
     wrapped to 32 bits, `float` registers), boxed only when they escape;
   - native loops for `for (i in a...b)` and `for (x in array)`;
   - array access with an `Int` index, inline bounds checks;
   - direct calls between native functions (static functions, constructors,
     `final` methods), fixed-arity entry points without the argument list.
6. **arm64**: text assembler, same CI comparison (`clang` on macOS arm64,
   GNU `as` on Linux arm64); `MAP_JIT` handling.
7. **Inline caches** in native code for method calls on non-final methods and
   fields of anonymous structures (same design as `macro-jit.md` step 5: a
   prototype or shape and a slot, published as one immutable value, checked
   inline).
8. **Tuning**: batch size, backend options (linear scan, CSE…) against
   compile time.

Step 3 of `macro-jit.md` (immediate `VInt of int`) is compatible with this
plan and simplifies it: less boxing at the boundary between tiers and in
step 5. If both are done, it goes before step 5 here.

Expected, to be confirmed by step 0 and step 5: on typed code (calls,
integer arithmetic, loops, arrays) native code within 1–3× of node, against
20–40× today; on `Dynamic` and anonymous structures, bounded by the helpers
until step 7. Macro execution on hide: under 1 s on one domain, once the
compile time is paid; on a server, and with the disk cache, it is paid
once.

## 5. Risks

- **Coupling with the OCaml version.** `Cmm`, `Cmm_helpers` and the output of
  `emit.mlp` change between OCaml releases. Upgrading OCaml means running
  `vendor.py` again and adapting the translator and the encoders; on another
  version the native tier is disabled at build time. The closure tier always
  remains.
- **GC correctness.** A wrong machtype (`Val` vs `Int` vs `Addr`) corrupts the
  heap. Mitigations: `Cmm_invariants` in debug builds, the startup tag check,
  GC stress runs (`OCAMLRUNPARAM` with a tiny minor heap) of the test suites
  with `-D eval-native=1` in CI.
- **Compile time.** Every function used by macros is compiled: the cost of
  the backend on the functions the translator produces is measured in step 3
  (on OCaml code it is about 250 µs per function, section 7) and reduced by
  backend options, batching and the disk cache.
- **Non-reentrant backend.** All its global state is touched under one lock,
  saved and restored around each compilation.
- **Binary size and startup.** The vendored backend adds 5.2 MB to the
  stripped binary and 3 ms to the startup (step 0); trimming the vendored set
  (section 7) reduces both.
- **Platforms.** macOS hardened runtime (entitlement), W^X everywhere, icache
  on arm64. Platforms without a supported encoder keep the closure tier.
- **Licence.** `compiler-libs` is LGPL 2.1 with the OCaml linking exception,
  compatible with the GPL of the Haxe compiler.

## 6. Constraints and verification

Same as `macro-jit.md` section 5 (semantics, parallel macros, test suites,
Heaps and hide output, warm/cold fuzzing), plus:
- every suite run with `-D eval-native=0` and `1`, with identical results;
- the GC stress run above;
- the CI comparison of the encoders against the system assembler;
- the fuzzing (`tests/fuzz/fuzz.py`) with `-D eval-native=1`, so that
  functions are compiled, invalidated and recompiled by the server.

## 7. Step 0 results

Measured on Linux x86-64, 4 cores, idle machine, OCaml 5.3.0.

**Spike** (`libs/jit/test/spike.ml`): six functions written in Cmm (recursive
calls, allocation, calls of OCaml closures, exceptions raised and caught in
both directions, a static block of GC roots holding a heap closure), compiled
by the vendored backend inside the process, encoded, loaded and called as
ordinary OCaml closures. All checks pass, including under GC stress (minor
heap of 4k words, `Gc.full_major` and `Gc.compact` while 4 domains run the
generated code), on repeated runs.
- compile 1.0 ms for the 6 functions, assemble and load 0.1–0.25 ms;
- `fib 32`: 10.4–11.3 ms through the JIT, 8.9–12 ms compiled by ocamlopt
  (same backend: same code).

**Encoder** (`libs/jit/x86_encode.ml`), compared with GNU as by
`libs/jit/test/asmdiff.{ml,py}` (an ocamlopt driver assembling every unit
both ways): the OCaml compiler sources (utils, parsing, typing, lambda,
middle end with flambda, asmcomp, driver), the standard library, `unix` and
`str`: **287 units, 5.0 MB compared, no difference** (alignment padding and
relocated fields excepted). Speed: 9.6 MB/s of code, about 10 times faster
than the backend that produces it.

**Backend cost**: on 10 504 functions of the OCaml compiler sources, 247 µs
per function with linear scan (292 µs with graph coloring), for about 250
bytes of code per function (1 MB/s). Applied to the 12 202 functions of
hide, this would be about 3 s; the functions generated from Haxe code differ
in size and shape from OCaml ones, so the real figure is measured once the
translator exists (step 3).

**Linking into Haxe**: `compiler-libs` collides with Haxe modules (above);
the vendored, wrapped backend links. 143 modules, 120 k lines (of which 60 k
for the generated OCaml parser, pulled by `Path` → `Lexer` → `Parser`, and
the flambda types pulled by `Cmx_format`), built in 5 s. Haxe binary: 21.8 →
27.1 MB stripped (+24 %), startup (`haxe --version`) 10.8 → 14.0 ms.
Trimming candidates: cut `Lexer`/`Parser` (used by `Path` only to print
keywords) and the flambda export info.

**Benchmarks** (`tests/benchs/eval`, `run.py`, median of 3 runs, ms; same
checksums in every mode):

| case | interp | macro | js (node) | interp/js | macro/js |
|---|---|---|---|---|---|
| calls | 545 | 495 | 14 | 38.9× | 35.4× |
| objects | 581 | 640 | 21 | 27.7× | 30.5× |
| arrays | 672 | 705 | 67 | 10.0× | 10.5× |
| anon | 702 | 730 | 18 | 39.0× | 40.6× |
| closures | 732 | 927 | 21 | 34.9× | 44.1× |
| strings | 492 | 538 | 33 | 14.9× | 16.3× |
| maps | 771 | 874 | 349 | 2.2× | 2.5× |
| enums | 597 | 633 | 90 | 6.6× | 7.0× |
| shader | 710 | 768 | 150 | 4.7× | 5.1× |

The `shader` case (lexer, parser, type checker and linker of a small shader
language, shaped like `hxsl.Macros.buildShader`) is only 4.7× slower than
node, against 28–40× for calls, objects and anonymous structures: on code
like hxsl, the time is spread over strings, maps, enum matching and
allocation, done by OCaml code of the eval runtime, which a JIT of the
interpreted code does not speed up. The gain on real macros must be estimated
from their profile, not from the micro benchmarks.

**hide** (`haxe6-annotations` branches of hide, heaps, hxbit, domkit, castle,
hscript, format and hashlink; first part of `hide.hxml`, `--times
-D macro-times -D parallel-typing=0`, 3 runs): total 10.1–10.2 s, macro
execution 1.83–2.01 s, of which `hxsl.Macros.buildShader` 1.04–1.14 s and
`hrt.prefab.EditContext2.build` 0.43 s; closure compilation of the 12 202
functions 0.06–0.09 s.

Profile of macro execution (perf, DWARF call graphs, samples whose stack is
in the execution of eval closures; samples in the GC lose their stack at
`caml_call_gc` and are not counted):

| where | share |
|---|---|
| interpreter: emitters and `EvalJit` closures, field access by hash (`EvalField`, `EvalHash`), prototypes, method calls, operators | 63 % |
| compiler, through the macro API (`MacroApi`, lexer and parser of strings, encoding of values) | 29 % |
| eval standard library (strings, arrays, maps) | 7 % |

Native code addresses the first line, and part of the allocation not shown
here; the macro API is unchanged by it.

**Consequences for the plan**:
- section 3.2 is now vendoring, not `compiler-libs`;
- no tiering: functions are compiled ahead of time where eval compiles them
  to closures today, with a disk cache later (section 3.5);
- in the interpreter share of hide, field access by name (hashing, prototype
  lookup) and method calls come first: inline caches (step 7) matter as much
  as typed arithmetic (step 5).

## 8. Native translation results and decision

The translation of step 3 is implemented (`src/macro/eval/evalNative.enabled.ml`,
`-D eval-native`): functions compiled ahead of time by groups (the methods of
a type in one unit), most expressions native, the others compiled by EvalJit
and called from the native code; Int values whose representation is known
kept unboxed; `tests/unit` passes with it, also under GC stress.

Measured on hide (macro execution 1.67–1.80 s with closures):

| variant | native code | compile time | macro execution |
|---|---|---|---|
| one unit per function | 1.6 MB | 2.6 s | no change |
| groups per type | 1.6 MB | 1.7 s | no change |
| + more expressions native | 2.6 MB | 2.8 s | 1.57–1.62 s |
| + Int specialization, merged calls | 2.5 MB | 2.9–3.0 s | 1.67–1.73 s |

- The OCaml backend costs about 1 µs per byte of code (3–4 µs per
  instruction, as much for a test as for a call), whatever the code: on
  OCaml's own sources as on the generated code.
- The time of macros is not in the dispatch of the closure tree, which the
  native code removes, but in what the operations do: field lookups by hash,
  prototypes, boxed integers (`VInt32` of a boxed `int32`), generic
  comparisons. The native code calls the same OCaml functions.
- Int specialization does not reduce the code on hide (little Int code, and
  boxing inline is larger than a call); merging the sequence of calls with
  simple operands reduces it by 10 %.

Decision: the native tier is left in place, disabled by default, and the
work goes to the closure tier (`macro-jit.md`): immediate integers (step 3)
and inline caches (step 5) address the costs above without compile time.
