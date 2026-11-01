# Debugging Intermittent Native Crashes (cpp/cppia)

Notes for the rare case of a native crash in CI that only happens some of the time. This file is not loaded automatically; point an agent to it when needed.

Lessons from tracking down a crash that hit about 1 in 3 `mac-test (cpp, arm64)` jobs (#13105, fixed in #13106):

- **Read the crash signature first.** With `HXCPP_CATCH_SEGV` (used by the test hosts), a SIGSEGV becomes a Haxe exception `hxSehException`; other signals such as SIGBUS or SIGABRT are not converted. The Haxe stack shows which phase crashed: in `cpp/cppia/Host.hx`, `Module.fromData` (parse, link and, with `-jit`, JIT compilation of the whole module) is a different line from `module.boot()`/`module.run()`. If the interpreted run passes and the `-jit` run crashes during `fromData`, the problem is in the JIT compile pass, before any script code runs.
- **Check when it started.** CI installs hxcpp from git master, so relate the first failures to hxcpp commits as well as Haxe commits. Count failures over many runs before blaming a recent change.
- **Get a native backtrace from CI.** Temporarily reduce the workflow to the failing job: set `if: false` on `windows64-build` and `linux-build`, which skips their dependent jobs, and cut down the `mac-test` matrix. Both `mac-build` arches are still needed for the universal binary. Then loop the failing command under `lldb --batch -o run -k "thread backtrace all" -k "frame variable *this" -k "quit 1" -- <host> <args>`.
  - **lldb disables ASLR by default.** A layout-dependent bug may never reproduce under it. Pass `-O "settings set target.disable-aslr false"`.
  - **Use Guard Malloc for heap bugs.** `settings set target.env-vars DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib` turns random heap overflows and use-after-frees into a deterministic fault at the first bad access; it is what pinpointed #13106. `MallocScribble=1` fills freed memory with `0x55`, which makes stale reads recognizable.
  - The `lldb.macosx.heap` `malloc_info --stack-history` command did not work on the macOS 26 runners.
- **cppia invariant:** hxcpp's `CppiaFunction::load` casts every class method body to `ScriptCallable *` (a `FUN` expression) without checking. The interpreter only uses it through virtual methods, but the JIT calls non-virtual members on it. The cppia generator must therefore emit a `FUN` for every method body, including abstract methods, or the JIT reads out of bounds.
- **Job logs are often too large to read in one go.** Fetch them with a bounded `tail_lines` (or save them to a file) and search for `stop reason`, `hxSehException` or `Command exited with`, rather than reading them in full.
