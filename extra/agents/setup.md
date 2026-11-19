# Setting Up the Toolchain, Haxelib and hxcpp

## Setting Up a Fresh Container
`extra/agent-setup.sh` sets up everything on Ubuntu 22.04 or newer: it installs the system packages, puts a prebuilt OCaml 5 toolchain with all opam dependencies in `/opt/opam` (building it from source only if no prebuilt one matches), adds the opam environment to `~/.bashrc`, runs `make haxe` and `make haxelib`, links both into `/usr/local/bin`, and installs the haxelib libraries the tests need (utest, hxnodejs, hxcoro, hxcpp, hxjava, haxeserver), bootstrapping hxcpp. Don't set up OCaml or opam by hand.

In a cloud agent container, the environment's setup script usually ran it before the session started, but the container image may come from an older version of the script or from a run with options that skip parts of it. Check what is there before relying on it:

```bash
export OPAMROOT=/opt/opam; eval "$(opam env)"   # needed in every new shell: agent harnesses often keep their own PATH, so the block in ~/.bashrc does not reach them
dune --version && ./haxe -version && haxelib path utest
```

If any of these fail, run `extra/agent-setup.sh` from the repository root (`--no-apt` if the system packages are already installed). It is safe to rerun: it keeps an opam root and libraries that are already set up and completes the rest in a few minutes.
- Pass `--no-build` to only set up the toolchain, or `--no-haxelib` to skip haxelib and its libraries
- Building haxelib by hand additionally needs the git submodules (`git submodule update --init --recursive`) before `make haxelib`

Anywhere else (other systems, Windows, manual setups), follow `extra/BUILDING.md`. The compiler needs OCaml 5.0+ with dune 3.17+ (managed via opam; `opam install haxe --deps-only` installs the OCaml dependencies after pinning the local checkout), the native libraries PCRE2, zlib and mbedTLS 3.x (4.x not yet supported), and the Neko VM for building haxelib. `make` builds everything; on Windows use `Makefile.win`.

## Which `haxe` Runs
- `-lib` needs `haxelib` on PATH, and haxelib in turn runs `haxe` from PATH; `extra/agent-setup.sh` links both into `/usr/local/bin`, unless an installed haxe or haxelib is already there (it then warns and leaves it alone)
- When in doubt, check with `haxe -version` or `Get-Command haxe` / `which haxe`

## Haxelib
- After building, the `haxe` and `haxelib` executables are in the repository root
- Run `haxelib setup` to set up the repository path
- Use `haxelib git utest https://github.com/haxe-utest/utest` for utest
- A git-cloned library can be set as a development library via `haxelib dev libname path`

## hxcpp
- **Bootstrapping**: The git version of hxcpp requires bootstrapping before C++ compilation works. Run `cd $(haxelib libpath hxcpp)tools/hxcpp && haxe compile.hxml` after installing. Without this, `haxelib run hxcpp` will prompt for interactive input and hang. (`extra/agent-setup.sh` does this automatically.)
- C++ compilation is slow (minutes for a full build with all static libraries) but should produce constant output. If it hangs with no output, hxcpp likely needs bootstrapping.
