#!/bin/bash
# Prepares a Claude Code cloud session for working on the Haxe compiler:
# OCaml toolchain and libraries (opam), the compiler build, and the tools
# used by tests/incremental and tests/unit (haxelib with utest; node is
# preinstalled in cloud containers).
set -euo pipefail

if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
	exit 0
fi

cd "$CLAUDE_PROJECT_DIR"

OCAML_VERSION=5.3.0
export OPAMYES=1 OPAMROOTISOK=1 OPAMCOLOR=never

# System packages (same list as the Linux job of .github/workflows/main.yml,
# plus neko and its headers for conf-neko and haxelib, and opam's own build
# requirements).
missing=""
for pkg in opam libpcre2-dev zlib1g-dev libmbedtls-dev neko neko-dev pkg-config m4 unzip git; do
	dpkg -s "$pkg" >/dev/null 2>&1 || missing="$missing $pkg"
done
if [ -n "$missing" ]; then
	SUDO=""
	[ "$(id -u)" -ne 0 ] && SUDO="sudo"
	$SUDO apt-get update -qq
	DEBIAN_FRONTEND=noninteractive $SUDO apt-get install -qqy $missing
fi

# OCaml switch with the compiler's dependencies (haxe.opam).
if [ ! -d "$HOME/.opam" ]; then
	opam init -a --disable-sandboxing --bare
fi
if ! opam switch list --short 2>/dev/null | grep -qx "$OCAML_VERSION"; then
	opam switch create "$OCAML_VERSION" "ocaml-base-compiler.$OCAML_VERSION"
fi
eval "$(opam env --switch="$OCAML_VERSION" --set-switch)"
opam pin add haxe . --no-action --kind=path >/dev/null
opam install haxe --deps-only --assume-depexts

# Build the compiler (incremental through dune, fast when up to date) and
# haxelib.
make -s -j"$(nproc)" haxe
git submodule update --init --recursive extra/haxelib_src
make -s haxelib

# haxelib repository with the libraries needed by tests/unit (as in
# tests/RunCi.hx).
export PATH="$CLAUDE_PROJECT_DIR:$PATH"
export HAXE_STD_PATH="$CLAUDE_PROJECT_DIR/std"
HAXELIB_REPO="$HOME/haxelib"
mkdir -p "$HAXELIB_REPO"
./haxelib setup "$HAXELIB_REPO" >/dev/null
if ! ./haxelib path utest >/dev/null 2>&1; then
	./haxelib git utest https://github.com/haxe-utest/utest >/dev/null
fi

# Make the OCaml environment and the freshly built compiler available to the
# session's shell commands.
if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
	opam env --switch="$OCAML_VERSION" --set-switch >> "$CLAUDE_ENV_FILE"
	echo "export PATH=\"$CLAUDE_PROJECT_DIR:\$PATH\"" >> "$CLAUDE_ENV_FILE"
	echo "export HAXE_STD_PATH=\"$CLAUDE_PROJECT_DIR/std\"" >> "$CLAUDE_ENV_FILE"
fi
