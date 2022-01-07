VERSION 0.6
FROM ubuntu:20.04
WORKDIR /tmp
    
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
    DO +INSTALL_PACKAGES --PACKAGES="curl"
    
    ARG TARGETARCH
    RUN set -ex && \
        case "$TARGETARCH" in \
            amd64) PLATFORM=linux64;; \
            arm64) PLATFORM=linux-arm64;; \
            *) exit 1;; \
        esac && \
        curl -sSL https://build.haxe.org/builds/neko/$PLATFORM/neko_latest.tar.gz -o neko_latest.tar.gz && \
        tar -xf neko_latest.tar.gz && \
        mv `echo neko-*-*` /tmp/neko-unpacked
    
    SAVE ARTIFACT /tmp/neko-unpacked/*
    SAVE IMAGE --cache-hint
    
build-environment:
    DO +INSTALL_NEKO
    
    RUN set -ex && \
        apt-get update -qqy && \
        apt-get install -qqy software-properties-common && \
        add-apt-repository ppa:avsm/ppa -y && \
        add-apt-repository ppa:haxe/ocaml -y && \
        apt-get update -qqy && \
        apt-get install -qqy ocaml-nox camlp5 opam libpcre3-dev zlib1g-dev libgtk2.0-dev libmbedtls-dev ninja-build libstring-shellquote-perl libstring-shellquote-perl libipc-system-simple-perl && \
        apt-get autoremove -y && \
        apt-get clean -y
        
    SAVE IMAGE --cache-hint
        
build:
    FROM +build-environment
    
    ARG TARGETPLATFORM
    ARG ADD_REVISION
    
    # Copy files
    COPY --dir extra libs plugins src* std dune* Makefile* opam .
    
    # Install OCaml libraries
    RUN set -ex && \
        opam init --disable-sandboxing && \
        opam update && \
        opam pin add haxe . --no-action && \
        opam install haxe --yes --deps-only --assume-depexts && \
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
    ENV NEKOPATH=/tmp/neko
    ENV HAXE_STD_PATH=/tmp/haxe/std
    DO +INSTALL_NEKO --NEKOPATH=$NEKOPATH
    DO +INSTALL_HAXE --HAXE_STD_PATH=$HAXE_STD_PATH
    
    ENV DEBIAN_FRONTEND=noninteractive
    ENV COMMON_PACKAGES=wget git build-essential locales sqlite3
    
    # Node.js is required as there are tests that use it (search "-cmd node")
    RUN curl -fsSL https://deb.nodesource.com/setup_16.x | bash - && \
        apt-get install -qqy nodejs $COMMON_PACKAGES && \
        apt-get autoremove -y && \
        apt-get clean -y
    
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
    BUILD +test --TEST=js
    BUILD +test --TEST=cpp
    BUILD +test --TEST=lua
    
    IF [ "$TARGETPLATFORM" = "linux/amd64" ] # FIXME: hl can't compile on arm64 (JIT issue?)
        BUILD +test --TEST=hl
    END