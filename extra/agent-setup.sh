#!/usr/bin/env bash
# Sets up an OCaml toolchain for building the Haxe compiler on Ubuntu, then builds it.
# Used by CI (linux-build in .github/workflows/main.yml), by coding agents in fresh
# containers (Claude Code, Copilot, ...), and by anyone on Ubuntu 22.04 or newer.
#
# Building OCaml 5 and the opam dependencies from source takes 5-20 minutes, so the
# script first tries to download a prebuilt opam root. CI's linux-build job builds that
# root on Ubuntu 22.04 when none matches the current key, and publishes it from pushes
# to `development` as an asset of the `opam-root` prerelease.
#
# Usage: extra/agent-setup.sh [options]
#   --from-source  never download the prebuilt opam root
#   --no-apt       don't install system packages (the caller has done that)
#   --no-build     only set up the toolchain, don't run `make haxe`
#   --no-profile   don't add the opam environment to ~/.bashrc and ~/.profile
#   --print-asset  print the file name of the matching prebuilt opam root and exit
#   --print-url    print the download URL of the matching prebuilt opam root and exit
#
# Environment:
#   OPAM_ROOT_RELEASE_URL  where to download the prebuilt opam root from
#   OPAMROOT  where the opam root lives (default /opt/opam; the prebuilt archive only
#             works at that path, because opam roots are not relocatable)

set -euo pipefail

OCAML_VERSION=5.3.0
# The OS the prebuilt archive is built on. It also works on newer Ubuntu releases.
BUILD_OS=ubuntu-22.04
# Bump to force a rebuild, e.g. to pick up newer versions of the dependencies.
REVISION=1
DEFAULT_OPAMROOT=/opt/opam
RELEASE_URL=${OPAM_ROOT_RELEASE_URL:-https://github.com/HaxeFoundation/haxe/releases/download/opam-root}

REPO_ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
export OPAMROOT=${OPAMROOT:-$DEFAULT_OPAMROOT}
export OPAMYES=1 OPAMROOTISOK=1 OPAMCOLOR=never

FROM_SOURCE=0 APT=1 BUILD=1 PROFILE=1 PRINT=
for arg in "$@"; do
	case "$arg" in
		--from-source) FROM_SOURCE=1 ;;
		--no-apt) APT=0 ;;
		--no-build) BUILD=0 ;;
		--no-profile) PROFILE=0 ;;
		--print-asset) PRINT=asset ;;
		--print-url) PRINT=url ;;
		*) echo "unknown option: $arg" >&2; exit 2 ;;
	esac
done

log() { echo "[agent-setup] $*" >&2; }

# The key changes whenever something that affects the contents of the opam root changes.
cache_key() {
	{
		echo "$OCAML_VERSION $BUILD_OS $REVISION"
		cat "$REPO_ROOT/haxe.opam"
	} | sha256sum | cut -c1-16
}
ASSET="opam-$OCAML_VERSION-$BUILD_OS-$(uname -m)-$(cache_key).tar.zst"

case "$PRINT" in
	asset) echo "$ASSET"; exit 0 ;;
	url) echo "$RELEASE_URL/$ASSET"; exit 0 ;;
esac

SUDO=
if [ "$(id -u)" != 0 ]; then SUDO=sudo; fi

install_system_packages() {
	log "installing system packages"
	export DEBIAN_FRONTEND=noninteractive
	$SUDO apt-get update -qq
	$SUDO apt-get install -qqy --no-install-recommends \
		opam git curl ca-certificates zstd make gcc g++ m4 pkg-config unzip bzip2 rsync patch \
		libpcre2-dev zlib1g-dev libzstd-dev libmbedtls-dev neko neko-dev >/dev/null
}

opam_root_ready() {
	[ -x "$OPAMROOT/$OCAML_VERSION/bin/ocamlfind" ] && [ -x "$OPAMROOT/$OCAML_VERSION/bin/dune" ] &&
		"$OPAMROOT/$OCAML_VERSION/bin/ocamlopt" -version >/dev/null 2>&1
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
	if ! opam_root_ready; then
		log "the prebuilt opam root doesn't work on this system"
		$SUDO rm -rf "$OPAMROOT"
		return 1
	fi
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

if [ "$APT" = 1 ]; then install_system_packages; fi
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
