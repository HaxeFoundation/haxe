#!/usr/bin/env bash
# Sets up an OCaml toolchain for building the Haxe compiler on Ubuntu, then builds it.
# Meant for coding agents in fresh containers (Claude Code, Copilot, ...), but works for
# anyone on Ubuntu 22.04 or newer.
#
# Building OCaml 5 and the opam dependencies from source takes 5-20 minutes, so the
# script first tries to download the opam root that CI's linux-build job builds on
# Ubuntu 22.04 and publishes from pushes to `development`, as an asset of the
# `opam-root` prerelease. It only builds from source if no published archive matches.
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

# Keep in sync with OCAML_VERSION in .github/workflows/main.yml.
OCAML_VERSION=5.3.0
# The OS the prebuilt archive is built on (linux-build's runner). It also works on newer
# Ubuntu releases.
BUILD_OS=ubuntu-22.04
# Bump to publish a fresh archive, e.g. to pick up newer versions of the dependencies.
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
	[ -f "$OPAMROOT/config" ] && opam exec -- ocamlfind ocamlopt -version >/dev/null 2>&1 &&
		opam exec -- dune --version >/dev/null 2>&1
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

# Same steps as linux-build in .github/workflows/main.yml.
build_from_source() {
	log "building OCaml $OCAML_VERSION and the dependencies from source (this takes a while)"
	$SUDO mkdir -p "$OPAMROOT"
	if [ -n "$SUDO" ]; then $SUDO chown "$(id -u):$(id -g)" "$OPAMROOT"; fi
	# Reuse an existing opam root (e.g. a half-finished earlier run) instead of wiping it.
	if [ ! -f "$OPAMROOT/config" ]; then
		# Containers usually can't run opam's bubblewrap sandbox.
		opam init -c "$OCAML_VERSION" --no-setup --disable-sandboxing
	fi
	opam pin add haxe "$REPO_ROOT" --no-action
	opam install haxe --deps-only --assume-depexts
	opam clean --all-switches --download-cache --logs --repo-cache
}

write_profile() {
	local marker="# haxe agent-setup"
	local block="$marker
export OPAMROOT=$OPAMROOT OPAMROOTISOK=1
eval \"\$(opam env 2>/dev/null)\""
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
eval "$(opam env)"
log "OCaml $(ocamlfind ocamlopt -version), dune $(dune --version)"

if [ "$PROFILE" = 1 ]; then write_profile; fi

if [ "$BUILD" = 1 ]; then
	log "building the compiler"
	make -C "$REPO_ROOT" -s -j"$(nproc)" haxe
	"$REPO_ROOT/haxe" -version
fi
log "done"
