#!/usr/bin/env bash
# Sets up an OCaml toolchain for building the Haxe compiler on Ubuntu, then builds it.
# Meant for fresh containers used by coding agents (Claude Code, Copilot, ...),
# but works for anyone on Ubuntu 24.04.
#
# Building OCaml 5 and the opam dependencies from source takes 10-20 minutes, so the
# script first tries to download a prebuilt opam root published by
# .github/workflows/agent-ocaml-deps.yml, and only builds from source if none matches.
#
# Usage: extra/agent-setup.sh [options]
#   --from-source  never download the prebuilt opam root
#   --no-build     only set up the toolchain, don't run `make haxe`
#   --no-profile   don't add the opam environment to ~/.bashrc and ~/.profile
#   --print-key    print the cache key of the prebuilt opam root and exit
#
# Environment:
#   OPAMROOT  where the opam root lives (default /opt/opam; the prebuilt archive only
#             works at that path, because opam roots are not relocatable)

set -euo pipefail

OCAML_VERSION=5.3.0
DEFAULT_OPAMROOT=/opt/opam
RELEASE_URL=https://github.com/HaxeFoundation/haxe/releases/download/agent-ocaml-deps

REPO_ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
export OPAMROOT=${OPAMROOT:-$DEFAULT_OPAMROOT}
export OPAMYES=1 OPAMROOTISOK=1 OPAMCOLOR=never

FROM_SOURCE=0 BUILD=1 PROFILE=1
for arg in "$@"; do
	case "$arg" in
		--from-source) FROM_SOURCE=1 ;;
		--no-build) BUILD=0 ;;
		--no-profile) PROFILE=0 ;;
		--print-key) PRINT_KEY=1 ;;
		*) echo "unknown option: $arg" >&2; exit 2 ;;
	esac
done

log() { echo "[agent-setup] $*" >&2; }

# The key changes whenever something that affects the contents of the opam root changes.
cache_key() {
	. /etc/os-release
	{
		echo "$OCAML_VERSION"
		echo "$ID-$VERSION_ID"
		cat "$REPO_ROOT/haxe.opam"
	} | sha256sum | cut -c1-16
}
ASSET="opam-$(cache_key)-linux-$(uname -m).tar.zst"

if [ "${PRINT_KEY:-0}" = 1 ]; then
	echo "$ASSET"
	exit 0
fi

SUDO=
if [ "$(id -u)" != 0 ]; then SUDO=sudo; fi

install_system_packages() {
	log "installing system packages"
	export DEBIAN_FRONTEND=noninteractive
	$SUDO apt-get update -qq
	$SUDO apt-get install -qqy --no-install-recommends \
		opam git curl ca-certificates zstd make gcc g++ m4 pkg-config unzip bzip2 rsync patch \
		libpcre2-dev zlib1g-dev libmbedtls-dev neko neko-dev >/dev/null
}

opam_root_ready() {
	[ -x "$OPAMROOT/$OCAML_VERSION/bin/ocamlfind" ] && [ -x "$OPAMROOT/$OCAML_VERSION/bin/dune" ]
}

download_prebuilt() {
	if [ "$OPAMROOT" != "$DEFAULT_OPAMROOT" ]; then
		log "OPAMROOT is not $DEFAULT_OPAMROOT, skipping the prebuilt download"
		return 1
	fi
	log "downloading $RELEASE_URL/$ASSET"
	local tmp
	tmp=$(mktemp -d)
	if ! curl -fsSL --retry 3 --retry-delay 2 -o "$tmp/$ASSET" "$RELEASE_URL/$ASSET"; then
		log "no prebuilt opam root for this configuration"
		rm -rf "$tmp"
		return 1
	fi
	$SUDO mkdir -p "$(dirname "$OPAMROOT")"
	$SUDO rm -rf "$OPAMROOT"
	$SUDO tar -C "$(dirname "$OPAMROOT")" --zstd -xf "$tmp/$ASSET"
	rm -rf "$tmp"
	if [ -n "$SUDO" ]; then $SUDO chown -R "$(id -u):$(id -g)" "$OPAMROOT"; fi
}

build_from_source() {
	log "building OCaml $OCAML_VERSION and the dependencies from source (this takes a while)"
	$SUDO mkdir -p "$OPAMROOT"
	if [ -n "$SUDO" ]; then $SUDO chown "$(id -u):$(id -g)" "$OPAMROOT"; fi
	# Containers usually can't run opam's bubblewrap sandbox.
	if [ ! -f "$OPAMROOT/config" ]; then
		opam init --bare --no-setup --disable-sandboxing
	fi
	if ! opam switch list --short | grep -qx "$OCAML_VERSION"; then
		opam switch create "$OCAML_VERSION" "ocaml-base-compiler.$OCAML_VERSION"
	fi
	opam pin add haxe "$REPO_ROOT" --no-action --switch "$OCAML_VERSION"
	opam install haxe --deps-only --assume-depexts --switch "$OCAML_VERSION"
	opam clean --all-switches --download-cache --logs --repo-cache
}

write_profile() {
	local marker="# haxe agent-setup"
	local block="$marker
export OPAMROOT=$OPAMROOT OPAMROOTISOK=1
eval \"\$(opam env --switch=$OCAML_VERSION --set-switch 2>/dev/null)\""
	for f in "$HOME/.bashrc" "$HOME/.profile"; do
		if ! grep -qF "$marker" "$f" 2>/dev/null; then
			printf '\n%s\n' "$block" >> "$f"
			log "added the opam environment to $f"
		fi
	done
}

install_system_packages
if opam_root_ready; then
	log "opam root at $OPAMROOT is already set up"
elif [ "$FROM_SOURCE" = 1 ] || ! download_prebuilt; then
	build_from_source
fi
eval "$(opam env --switch="$OCAML_VERSION" --set-switch)"
log "$(ocamlfind ocamlopt -version 2>/dev/null | sed 's/^/OCaml /'), dune $(dune --version)"

if [ "$PROFILE" = 1 ]; then write_profile; fi

if [ "$BUILD" = 1 ]; then
	log "building the compiler"
	make -C "$REPO_ROOT" -s -j"$(nproc)" haxe
	"$REPO_ROOT/haxe" -version
fi
log "done"
