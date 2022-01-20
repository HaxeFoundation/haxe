VERSION 0.6
ARG UBUNTU_RELEASE=bionic
FROM mcr.microsoft.com/vscode/devcontainers/base:0-$UBUNTU_RELEASE
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

    DO +INSTALL_NEKO --NEKOPATH=$NEKOPATH

    COPY +earthly/earthly /usr/local/bin/
    RUN earthly bootstrap --no-buildkit --with-autocomplete

    USER $USER

    # Do not show git branch in bash prompt
    # It's slow
    RUN sed -ir 's/^__bash_prompt$/PS1="\\[\\033[0;32m\\]\\u \\[\\033[0m\\]➜ \\[\\033[1;34m\\]\\w\\[\\033[0m\\]\\$ "/' ~/.bashrc

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
    RUN curl -fsSL https://github.com/earthly/earthly/releases/download/v0.6.2/earthly-linux-${TARGETARCH} -o /usr/local/bin/earthly \
        && chmod +x /usr/local/bin/earthly
    SAVE ARTIFACT /usr/local/bin/earthly

INSTALL_PACKAGES:
    COMMAND
    ARG PACKAGES
    RUN set -ex && \
        apt-get update -qqy && \
        apt-get install -qqy $PACKAGES && \
        apt-get autoremove -y && apt-get clean -y && rm -rf /var/lib/apt/lists/*

INSTALL_NEKO:
    COMMAND
    ARG NEKOPATH
    COPY +neko/* $NEKOPATH/
    RUN bash -c 'set -ex && ln -s $NEKOPATH/{neko,nekoc,nekoml,nekotools} /usr/local/bin/ && ln -s $NEKOPATH/libneko.* /lib/'

INSTALL_HAXE:
    COMMAND
    ARG HAXE_STD_PATH
    COPY +build/haxe +build/haxelib /usr/local/bin/
    COPY +build/std $HAXE_STD_PATH/
    RUN ls -lah $HAXE_STD_PATH/

try-neko:
    ENV NEKOPATH=/tmp/neko
    DO +INSTALL_NEKO --NEKOPATH=$NEKOPATH
    RUN neko -version
    RUN nekotools server

try-haxe:
    ENV NEKOPATH=/tmp/neko
    ENV HAXE_STD_PATH=/tmp/haxe/std
    DO +INSTALL_NEKO --NEKOPATH=$NEKOPATH
    DO +INSTALL_HAXE --HAXE_STD_PATH=$HAXE_STD_PATH
    RUN ls -lah $HAXE_STD_PATH/
    RUN haxe -version
    RUN haxelib

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

    ARG TARGETPLATFORM
    ARG ADD_REVISION

    # Copy files
    COPY --dir extra libs plugins src* std dune* Makefile* opam .

    # Install OCaml libraries
    RUN set -ex && \
        opam init --disable-sandboxing && \
        opam update && \
        opam pin add haxe . --no-action && \
        opam install haxe --yes --deps-only --no-depexts && \
        opam list && \
        ocamlopt -v

    # Build Haxe
    RUN set -ex && \
        eval $(opam env) && \
        opam config exec -- make -s -j`nproc` STATICLINK=1 haxe && \
        opam config exec -- make -s haxelib && \
        make -s package_unix && \
        ls -l out && \
        ldd -v ./haxe && \
        ldd -v ./haxelib

    SAVE ARTIFACT std
    SAVE ARTIFACT ./out/* AS LOCAL out/$TARGETPLATFORM/
    SAVE ARTIFACT ./haxe* AS LOCAL out/$TARGETPLATFORM/
    SAVE IMAGE --cache-hint

build-multiarch:
    ARG ADD_REVISION
    BUILD --platform=linux/amd64 --platform=linux/arm64 +build --ADD_REVISION=$ADD_REVISION

xmldoc:
    DO +INSTALL_PACKAGES --PACKAGES="git"

    ENV NEKOPATH=/tmp/neko
    ENV HAXE_STD_PATH=/tmp/haxe/std
    DO +INSTALL_NEKO --NEKOPATH=$NEKOPATH
    DO +INSTALL_HAXE --HAXE_STD_PATH=$HAXE_STD_PATH

    COPY --dir extra .

    ARG COMMIT
    ARG BRANCH

    RUN set -ex                                                      && \
        cd extra                                                     && \
        haxelib newrepo                                              && \
        haxelib git hxcpp  https://github.com/HaxeFoundation/hxcpp   && \
        haxelib git hxjava https://github.com/HaxeFoundation/hxjava  && \
        haxelib git hxcs   https://github.com/HaxeFoundation/hxcs    && \
        haxe doc.hxml                                                && \
        echo "{\"commit\":\"$COMMIT\",\"branch\":\"$BRANCH\"}" > doc/info.json

    SAVE ARTIFACT ./extra/doc/* AS LOCAL extra/doc/

test-environment:
    FROM ubuntu:$UBUNTU_RELEASE
    ENV NEKOPATH=/tmp/neko
    ENV HAXE_STD_PATH=/tmp/haxe/std
    DO +INSTALL_NEKO --NEKOPATH=$NEKOPATH
    DO +INSTALL_HAXE --HAXE_STD_PATH=$HAXE_STD_PATH

    ENV DEBIAN_FRONTEND=noninteractive
    DO +INSTALL_PACKAGES --PACKAGES="curl wget git build-essential locales sqlite3"

    # Node.js is required as there are tests that use it (search "-cmd node")
    RUN curl -fsSL https://deb.nodesource.com/setup_16.x | bash - && \
        apt-get install -qqy nodejs $COMMON_PACKAGES && \
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
    DO +INSTALL_PACKAGES --PACKAGES="cmake ninja-build libturbojpeg-dev libpng-dev zlib1g-dev libvorbis-dev"
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
        ENV PACKAGES=g++-multilib
    ELSE IF [ "$TARGETPLATFORM" = "linux/arm64" ]
        ENV PACKAGES=g++-multilib-arm-linux-gnueabi
    ELSE
        RUN echo "Unsupported platform $TARGETPLATFORM" && exit 1
    END

    DO +INSTALL_PACKAGES --PACKAGES=$PACKAGES
    SAVE IMAGE --cache-hint

test:
    ARG TEST # macro, js, hl, cpp, java ,jvm, cs, php, python, lua, neko

    FROM +test-environment

    IF [ "$TEST" = "python" ]
        FROM +test-environment-python
    ELSE IF [ "$TEST" = "php" ]
        FROM +test-environment-php
    ELSE IF [ "$TEST" = "java,jvm" ]
        FROM +test-environment-java
    ELSE IF [ "$TEST" = "js" ]
        FROM +test-environment-js
    ELSE IF [ "$TEST" = "cs" ]
        FROM +test-environment-cs
    ELSE IF [ "$TEST" = "cpp" ]
        FROM +test-environment-cpp
    ELSE IF [ "$TEST" = "lua" ]
        FROM +test-environment-lua
    ELSE IF [ "$TEST" = "hl" ]
        FROM +test-environment-hl
        ENV GITHUB_WORKSPACE=true # emulate github environment, TODO: properly define a "Earthly" environment
    END

    ENV HAXE_STD_PATH=/haxe/std
    WORKDIR /haxe

    COPY +build/haxe +build/haxelib /usr/local/bin/
    COPY --dir tests std .

    RUN mkdir /haxelib \
        && haxelib setup /haxelib \
        && cd tests \
        && haxe RunCi.hxml

test-all:
    ARG TARGETPLATFORM

    BUILD +test --TEST=macro
    BUILD +test --TEST=neko
    BUILD +test --TEST=php
    BUILD +test --TEST=python
    BUILD +test --TEST=java,jvm
    BUILD +test --TEST=cs
    BUILD +test --TEST=cpp
    BUILD +test --TEST=lua
    BUILD +test --TEST=js # FIXME: timeout

    IF [ "$TARGETPLATFORM" = "linux/amd64" ]
        BUILD +test --TEST=hl # FIXME: hl can't compile on arm64 (JIT issue?)
    END
