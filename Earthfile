VERSION 0.6
FROM ubuntu:20.04
WORKDIR /tmp

ENV NEKOPATH=/tmp/neko
        
neko:
    ARG TARGETPLATFORM
    
    IF [ "$TARGETPLATFORM" = "linux/amd64" ]
        ENV PLATFORM=linux64
    ELSE IF [ "$TARGETPLATFORM" = "linux/arm64" ]
        ENV PLATFORM=linux-arm64
    ELSE 
        RUN echo "Unsupported platform $TARGETPLATFORM" && exit 1
    END
    
    RUN apt-get update -qqy \
        && apt-get install -qqy curl
    
    RUN set -ex                                                                                                 && \
        curl -sSL https://build.haxe.org/builds/neko/$PLATFORM/neko_latest.tar.gz -o /tmp/neko_latest.tar.gz    && \
        tar -xf /tmp/neko_latest.tar.gz -C /tmp                                                                 && \
        mv `echo /tmp/neko-*-*` $NEKOPATH                                                                       && \
        mkdir -p /usr/local/bin                                                                                 && \
        mkdir -p /usr/local/lib/neko                                                                            && \
        ln -s $NEKOPATH/{neko,nekoc,nekoml,nekotools}  /usr/local/bin/                                          && \
        ln -s $NEKOPATH/libneko.*                      /lib/                                                    && \
        ln -s $NEKOPATH/*.ndll                         /usr/local/lib/neko/                                     && \
        PATH=$NEKOPATH:$PATH                                                                                    && \
        neko -version
    
build-environment:
    FROM +neko
    
    RUN set -ex && \
        apt-get update -qqy && \
        apt-get install -qqy software-properties-common && \
        add-apt-repository ppa:avsm/ppa -y && \
        add-apt-repository ppa:haxe/ocaml -y && \
        apt-get update -qqy && \
        apt-get install -qqy ocaml-nox camlp5 opam libpcre3-dev zlib1g-dev libgtk2.0-dev libmbedtls-dev ninja-build libstring-shellquote-perl libstring-shellquote-perl libipc-system-simple-perl
        
build:
    FROM +build-environment
    
    ARG TARGETPLATFORM
    ARG ADD_REVISION
    
    # Copy files
    COPY --dir extra libs plugins src* std dune* Makefile* opam .
    
    # Install OCaml libraries
    RUN set -ex && \
        export PATH=$NEKOPATH:$PATH && \
        opam init --disable-sandboxing && \
        opam update && \
        opam pin add haxe . --no-action && \
        opam install haxe --yes --deps-only --assume-depexts && \
        opam list && \
        ocamlopt -v
        
    # Build Haxe
    RUN set -ex && \
        export PATH=$NEKOPATH:$PATH && \
        eval $(opam env) && \
        opam config exec -- make -s -j`nproc` STATICLINK=1 haxe && \
        opam config exec -- make -s haxelib && \
        make -s package_unix && \
        ls -l out && \
        ldd -v ./haxe && \
        ldd -v ./haxelib
    
    SAVE ARTIFACT ./out/* AS LOCAL out/$TARGETPLATFORM/
    SAVE ARTIFACT ./haxe* AS LOCAL out/$TARGETPLATFORM/
    
build-multiarch:
    ARG ADD_REVISION
    BUILD --platform=linux/amd64 --platform=linux/arm64 +build --ADD_REVISION=$ADD_REVISION
    
xmldoc:
    FROM +build
    
    ARG COMMIT
    ARG BRANCH
    
    COPY . .
    
    RUN set -ex                                                         && \
        export PATH=$NEKOPATH:$PWD:$PATH                                && \
        cd extra                                                        && \
        ../haxelib newrepo                                              && \
        ../haxelib git hxcpp  https://github.com/HaxeFoundation/hxcpp   && \
        ../haxelib git hxjava https://github.com/HaxeFoundation/hxjava  && \
        ../haxelib git hxcs   https://github.com/HaxeFoundation/hxcs    && \
        ../haxe doc.hxml
        
    RUN echo "{\"commit\":\"$COMMIT\",\"branch\":\"$BRANCH\"}" > extra/doc/info.json
    
    SAVE ARTIFACT ./extra/doc/* AS LOCAL extra/doc/
    
# python-test-environment:
#     FROM python:3
#     ARG TARGETPLATFORM
#     DO +INSTALL_NEKO --TARGETPLATFORM=$TARGETPLATFORM --NEKOPATH=$NEKOPATH
    
test-environment:
    FROM +neko
    
    ENV DEBIAN_FRONTEND=noninteractive
    ENV COMMON_PACKAGES=wget git build-essential locales sqlite3
    
    # Node.js is required as there are tests that use it (search "-cmd node")
    RUN curl -fsSL https://deb.nodesource.com/setup_16.x | bash - \
        && apt-get install -qqy nodejs $COMMON_PACKAGES
    
    # set locale
    RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && locale-gen
    ENV LANG=en_US.UTF-8  
    ENV LANGUAGE=en_US:en  
    ENV LC_ALL=en_US.UTF-8
    
INSTALL_PACKAGES:
    COMMAND
    ARG PACKAGES
    RUN set -ex && \
        apt-get update -qqy && \
        apt-get install -qqy $PACKAGES && \
        apt-get clean
    
test-environment-java:
    FROM +test-environment
    DO +INSTALL_PACKAGES --PACKAGES="default-jdk"
    
test-environment-js:
    # somehow js tests require hxjava which in turns require javac
    FROM +test-environment-java 
    
test-environment-python:
    FROM +test-environment
    DO +INSTALL_PACKAGES --PACKAGES="python3"
    
test-environment-php:
    FROM +test-environment
    DO +INSTALL_PACKAGES --PACKAGES="php-cli php-mbstring php-sqlite3"
    
test-environment-cs:
    FROM +test-environment
    DO +INSTALL_PACKAGES --PACKAGES="mono-devel mono-mcs"
    
test-environment-hl:
    FROM +test-environment
    DO +INSTALL_PACKAGES --PACKAGES="cmake ninja-build libturbojpeg-dev libpng-dev zlib1g-dev libvorbis-dev"
    
test-environment-lua:
    # hererocks uses pip
    FROM +test-environment-python 
    DO +INSTALL_PACKAGES --PACKAGES="libssl-dev libreadline-dev python3-pip unzip libpcre3-dev cmake"
    RUN ln -s /root/.local/bin/hererocks /bin/
    
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
    
    COPY +build/* .
    COPY --dir tests std .
    
    RUN PATH=$PATH:$PWD:$NEKOPATH \
        && mkdir /haxelib \
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
    
    IF [ "$TARGETPLATFORM" = "linux/amd64" ] # FIXME
        BUILD +test --TEST=hl
    END