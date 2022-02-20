VERSION 0.6
FROM mcr.microsoft.com/vscode/devcontainers/base:0-bionic
ARG DEVCONTAINER_IMAGE_NAME_DEFAULT=ghcr.io/haxefoundation/haxe_devcontainer

ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

ARG WORKDIR=/workspace
RUN mkdir -m 777 "$WORKDIR"
WORKDIR "$WORKDIR"

ARG --required TARGETARCH

devcontainer-library-scripts:
    RUN curl -fsSLO https://raw.githubusercontent.com/microsoft/vscode-dev-containers/main/script-library/common-debian.sh
    RUN curl -fsSLO https://raw.githubusercontent.com/microsoft/vscode-dev-containers/main/script-library/docker-debian.sh
    SAVE ARTIFACT --keep-ts *.sh AS LOCAL .devcontainer/library-scripts/

devcontainer:
    # Avoid warnings by switching to noninteractive
    ENV DEBIAN_FRONTEND=noninteractive

    ARG INSTALL_ZSH="false"
    ARG UPGRADE_PACKAGES="true"
    ARG ENABLE_NONROOT_DOCKER="true"
    ARG USE_MOBY="false"
    COPY .devcontainer/library-scripts/common-debian.sh .devcontainer/library-scripts/docker-debian.sh /tmp/library-scripts/
    RUN apt-get update \
        && /bin/bash /tmp/library-scripts/common-debian.sh "${INSTALL_ZSH}" "${USERNAME}" "${USER_UID}" "${USER_GID}" "${UPGRADE_PACKAGES}" "true" "true" \
        && /bin/bash /tmp/library-scripts/docker-debian.sh "${ENABLE_NONROOT_DOCKER}" "/var/run/docker-host.sock" "/var/run/docker.sock" "${USERNAME}" "${USE_MOBY}" \
        # Clean up
        && apt-get autoremove -y && apt-get clean -y && rm -rf /var/lib/apt/lists/* /tmp/library-scripts/

    # Setting the ENTRYPOINT to docker-init.sh will configure non-root access
    # to the Docker socket. The script will also execute CMD as needed.
    ENTRYPOINT [ "/usr/local/share/docker-init.sh" ]
    CMD [ "sleep", "infinity" ]

    # Configure apt and install packages
    RUN apt-get update \
        && apt-get install -qqy --no-install-recommends apt-utils dialog 2>&1 \
        && apt-get install -qqy --no-install-recommends \
            iproute2 \
            procps \
            sudo \
            bash-completion \
            build-essential \
            curl \
            wget \
            software-properties-common \
            direnv \
            tzdata \
            # install docker engine for using `WITH DOCKER`
            docker-ce \
        # install node
        && curl -sL https://deb.nodesource.com/setup_16.x | bash - \
        && apt-get install -qqy --no-install-recommends nodejs=16.* \
        # install ocaml and other haxe compiler deps
        && add-apt-repository ppa:avsm/ppa \
        && add-apt-repository ppa:haxe/ocaml \
        && apt-get install -qqy --no-install-recommends \
            ocaml-nox \
            camlp5 \
            opam \
            libpcre3-dev \
            zlib1g-dev \
            libgtk2.0-dev \
            libmbedtls-dev \
            ninja-build \
            libstring-shellquote-perl \
            libipc-system-simple-perl \
        #
        # Clean up
        && apt-get autoremove -y \
        && apt-get clean -y \
        && rm -rf /var/lib/apt/lists/*

    # Switch back to dialog for any ad-hoc use of apt-get
    ENV DEBIAN_FRONTEND=

    DO +INSTALL_NEKO

    COPY +earthly/earthly /usr/local/bin/
    RUN earthly bootstrap --no-buildkit --with-autocomplete

    USER $USERNAME

    # Do not show git branch in bash prompt because it's slow
    # https://github.com/microsoft/vscode-dev-containers/issues/1196#issuecomment-988388658
    RUN git config --global codespaces-theme.hide-status 1

    # Install OCaml libraries
    COPY opam .
    RUN opam init --disable-sandboxing
    RUN opam install . --yes --deps-only --no-depexts
    RUN opam list
    RUN ocamlopt -v

    USER root

    ARG GIT_SHA
    ENV GIT_SHA="$GIT_SHA"
    ARG IMAGE_NAME="$DEVCONTAINER_IMAGE_NAME_DEFAULT"
    ARG IMAGE_TAG="development"
    ARG IMAGE_CACHE="$IMAGE_NAME:$IMAGE_TAG"
    SAVE IMAGE --cache-from="$IMAGE_CACHE" --push "$IMAGE_NAME:$IMAGE_TAG"

# Usage:
# COPY +earthly/earthly /usr/local/bin/
# RUN earthly bootstrap --no-buildkit --with-autocomplete
earthly:
    ARG --required TARGETARCH
    RUN curl -fsSL https://github.com/earthly/earthly/releases/download/v0.6.5/earthly-linux-${TARGETARCH} -o /usr/local/bin/earthly \
        && chmod +x /usr/local/bin/earthly
    SAVE ARTIFACT /usr/local/bin/earthly

INSTALL_PACKAGES:
    COMMAND
    ARG PACKAGES
    RUN apt-get update -qqy && \
        apt-get install -qqy --no-install-recommends $PACKAGES && \
        apt-get autoremove -y && apt-get clean -y && rm -rf /var/lib/apt/lists/*

INSTALL_NEKO:
    COMMAND
    ARG NEKOPATH=/neko
    COPY +neko/* "$NEKOPATH/"
    ARG PREFIX=/usr/local
    RUN bash -c "ln -s \"$NEKOPATH\"/{neko,nekoc,nekoml,nekotools} \"$PREFIX/bin/\""
    RUN bash -c "ln -s \"$NEKOPATH\"/libneko.* \"$PREFIX/lib/\""
    RUN mkdir -p "$PREFIX/lib/neko/"
    RUN bash -c "ln -s \"$NEKOPATH\"/*.ndll \"$PREFIX/lib/neko/\""
    RUN ldconfig

INSTALL_HAXE:
    COMMAND
    ARG PREFIX=/usr/local
    COPY +build/haxe +build/haxelib "$PREFIX/bin/"
    COPY std "$PREFIX/share/haxe/std"

try-neko:
    DO +INSTALL_NEKO
    RUN neko -version
    RUN nekotools

try-haxe:
    DO +INSTALL_NEKO
    DO +INSTALL_HAXE
    RUN haxe --version
    RUN haxelib version

neko:
    RUN set -ex && \
        case "$TARGETARCH" in \
            amd64) PLATFORM=linux64;; \
            arm64) PLATFORM=linux-arm64;; \
            *) exit 1;; \
        esac && \
        curl -fsSL https://build.haxe.org/builds/neko/$PLATFORM/neko_latest.tar.gz -o neko_latest.tar.gz && \
        tar -xf neko_latest.tar.gz && \
        mv `echo neko-*-*` /tmp/neko-unpacked
    SAVE ARTIFACT /tmp/neko-unpacked/*
    SAVE IMAGE --cache-hint

build:
    FROM +devcontainer

    USER $USERNAME

    # Build Haxe
    COPY --dir extra libs plugins src* std dune* Makefile* .
    COPY .git .git # the Makefile calls git to get commit sha
    ARG ADD_REVISION
    ENV ADD_REVISION=$ADD_REVISION
    RUN opam config exec -- make -s -j`nproc` STATICLINK=1 haxe && ldd -v ./haxe
    RUN opam config exec -- make -s haxelib && ldd -v ./haxelib
    RUN make -s package_unix && ls -l out

    ARG TARGETPLATFORM
    SAVE ARTIFACT --keep-ts ./out/* AS LOCAL out/$TARGETPLATFORM/
    SAVE ARTIFACT --keep-ts ./haxe AS LOCAL out/$TARGETPLATFORM/
    SAVE ARTIFACT --keep-ts ./haxelib AS LOCAL out/$TARGETPLATFORM/
    SAVE IMAGE --cache-hint

build-multiarch:
    ARG ADD_REVISION
    BUILD --platform=linux/amd64 --platform=linux/arm64 +build --ADD_REVISION=$ADD_REVISION

xmldoc:
    DO +INSTALL_NEKO
    DO +INSTALL_HAXE

    COPY --dir extra .

    WORKDIR extra
    RUN haxelib newrepo
    RUN haxelib git hxcpp  https://github.com/HaxeFoundation/hxcpp
    RUN haxelib git hxjava https://github.com/HaxeFoundation/hxjava
    RUN haxelib git hxcs   https://github.com/HaxeFoundation/hxcs
    RUN haxe doc.hxml

    ARG COMMIT
    ARG BRANCH
    RUN echo "{\"commit\":\"$COMMIT\",\"branch\":\"$BRANCH\"}" > doc/info.json

    SAVE ARTIFACT --keep-ts ./doc AS LOCAL extra/doc

test-environment:
    # we use a sightly newer ubuntu for easier installation of the target runtimes (e.g. php)
    FROM ubuntu:focal
    DO +INSTALL_NEKO
    DO +INSTALL_HAXE

    ENV DEBIAN_FRONTEND=noninteractive
    DO +INSTALL_PACKAGES --PACKAGES="ca-certificates curl wget git build-essential locales sqlite3"

    # Node.js is required as there are tests that use it (search "-cmd node")
    RUN curl -fsSL https://deb.nodesource.com/setup_16.x | bash - && \
        apt-get install -qqy nodejs && \
        apt-get autoremove -y && apt-get clean -y && rm -rf /var/lib/apt/lists/*

    # set locale
    RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && locale-gen
    ENV LANG=en_US.UTF-8
    ENV LANGUAGE=en_US:en
    ENV LC_ALL=en_US.UTF-8

    SAVE IMAGE --cache-hint

test-environment-java:
    FROM +test-environment
    DO +INSTALL_PACKAGES --PACKAGES="default-jdk"
    SAVE IMAGE --cache-hint

test-environment-js:
    # somehow js tests require hxjava which in turns require javac
    FROM +test-environment-java

test-environment-python:
    FROM +test-environment
    DO +INSTALL_PACKAGES --PACKAGES="python3"
    SAVE IMAGE --cache-hint

test-environment-php:
    FROM +test-environment
    DO +INSTALL_PACKAGES --PACKAGES="php-cli php-mbstring php-sqlite3"
    SAVE IMAGE --cache-hint

test-environment-cs:
    FROM +test-environment
    DO +INSTALL_PACKAGES --PACKAGES="mono-devel mono-mcs"
    SAVE IMAGE --cache-hint

test-environment-hl:
    FROM +test-environment
    DO +INSTALL_PACKAGES --PACKAGES="cmake ninja-build libturbojpeg-dev libpng-dev zlib1g-dev libvorbis-dev libsqlite3-dev"
    SAVE IMAGE --cache-hint

test-environment-lua:
    # hererocks uses pip
    FROM +test-environment-python
    DO +INSTALL_PACKAGES --PACKAGES="libssl-dev libreadline-dev python3-pip unzip libpcre3-dev cmake"
    RUN ln -s /root/.local/bin/hererocks /bin/
    SAVE IMAGE --cache-hint

test-environment-cpp:
    FROM +test-environment

    ARG TARGETPLATFORM

    IF [ "$TARGETPLATFORM" = "linux/amd64" ]
        DO +INSTALL_PACKAGES --PACKAGES="g++-multilib"
    ELSE IF [ "$TARGETPLATFORM" = "linux/arm64" ]
        DO +INSTALL_PACKAGES --PACKAGES="g++-multilib-arm-linux-gnueabi"
    ELSE
        RUN echo "Unsupported platform $TARGETPLATFORM" && exit 1
    END

    SAVE IMAGE --cache-hint

RUN_CI:
    COMMAND
    COPY tests tests
    RUN mkdir /haxelib && haxelib setup /haxelib
    WORKDIR tests
    ARG --required TEST
    ENV TEST="$TEST"
    RUN haxe RunCi.hxml

test-macro:
    FROM +test-environment
    ARG GITHUB_ACTIONS
    ENV GITHUB_ACTIONS=$GITHUB_ACTIONS
    DO +RUN_CI --TEST=macro

test-neko:
    FROM +test-environment
    ARG GITHUB_ACTIONS
    ENV GITHUB_ACTIONS=$GITHUB_ACTIONS
    DO +RUN_CI --TEST=neko

test-js:
    FROM +test-environment-js
    ARG GITHUB_ACTIONS
    ENV GITHUB_ACTIONS=$GITHUB_ACTIONS
    DO +RUN_CI --TEST=js

test-hl:
    FROM +test-environment-hl
    ARG GITHUB_ACTIONS
    ENV GITHUB_ACTIONS=$GITHUB_ACTIONS
    DO +RUN_CI --TEST=hl

test-cpp:
    FROM +test-environment-cpp
    ARG GITHUB_ACTIONS
    ENV GITHUB_ACTIONS=$GITHUB_ACTIONS
    DO +RUN_CI --TEST=cpp

test-java:
    FROM +test-environment-java
    ARG GITHUB_ACTIONS
    ENV GITHUB_ACTIONS=$GITHUB_ACTIONS
    DO +RUN_CI --TEST=java

test-jvm:
    FROM +test-environment-java
    ARG GITHUB_ACTIONS
    ENV GITHUB_ACTIONS=$GITHUB_ACTIONS
    DO +RUN_CI --TEST=jvm

test-cs:
    FROM +test-environment-cs
    ARG GITHUB_ACTIONS
    ENV GITHUB_ACTIONS=$GITHUB_ACTIONS
    DO +RUN_CI --TEST=cs

test-php:
    FROM +test-environment-php
    ARG GITHUB_ACTIONS
    ENV GITHUB_ACTIONS=$GITHUB_ACTIONS
    DO +RUN_CI --TEST=php

test-python:
    FROM +test-environment-python
    ARG GITHUB_ACTIONS
    ENV GITHUB_ACTIONS=$GITHUB_ACTIONS
    DO +RUN_CI --TEST=python

test-lua:
    FROM +test-environment-lua
    ARG GITHUB_ACTIONS
    ENV GITHUB_ACTIONS=$GITHUB_ACTIONS
    DO +RUN_CI --TEST=lua

test-all:
    ARG TARGETPLATFORM

    BUILD +test-macro
    BUILD +test-neko
    BUILD +test-php
    BUILD +test-python
    BUILD +test-java
    BUILD +test-jvm
    BUILD +test-cs
    BUILD +test-cpp
    BUILD +test-lua
    BUILD +test-js

    IF [ "$TARGETPLATFORM" = "linux/amd64" ]
        BUILD +test-hl # FIXME: hl can't compile on arm64 (JIT issue?)
    END

github-actions:
    DO +INSTALL_NEKO
    DO +INSTALL_HAXE
    RUN mkdir -p "$WORKDIR"/.github/workflows
    COPY extra/github-actions extra/github-actions
    WORKDIR extra/github-actions
    RUN haxe build.hxml
    SAVE ARTIFACT --keep-ts "$WORKDIR"/.github/workflows AS LOCAL .github/workflows
