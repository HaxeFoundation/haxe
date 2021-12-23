VERSION 0.6
FROM ubuntu:20.04
WORKDIR /tmp

ENV NEKOPATH=/tmp/neko

build-multiarch:
    ARG ADD_REVISION
    BUILD --platform=linux/amd64 --platform=linux/arm64 +build --ADD_REVISION=$ADD_REVISION
        
install-neko:
    FROM +install-dependencies
    
    ARG TARGETPLATFORM
    
    IF [ "$TARGETPLATFORM" = "linux/amd64" ]
        ENV PLATFORM=linux64
    ELSE IF [ "$TARGETPLATFORM" = "linux/arm64" ]
        ENV PLATFORM=linux-arm64
    ELSE 
        RUN echo "Unsupported platform $TARGETPLATFORM" && exit 1
    END
    
    RUN set -ex                                                                                                 && \
        curl -sSL https://build.haxe.org/builds/neko/$PLATFORM/neko_latest.tar.gz -o /tmp/neko_latest.tar.gz    && \
        tar -xf /tmp/neko_latest.tar.gz -C /tmp                                                                 && \
        mv `echo /tmp/neko-*-*` $NEKOPATH                                                                       && \
        mkdir -p /usr/local/bin                                                                                 && \
        mkdir -p /usr/local/lib/neko                                                                            && \
        ln -s $NEKOPATH/{neko,nekoc,nekoml,nekotools}  /usr/local/bin/                                          && \
        ln -s $NEKOPATH/libneko.*                      /usr/local/lib/                                          && \
        ln -s $NEKOPATH/*.ndll                         /usr/local/lib/neko/                                     && \
        PATH=$NEKOPATH:$PATH                                                                                    && \
        neko -version
    
install-dependencies:
    RUN set -ex && \
        apt-get update -qqy && \
        apt-get install -qqy software-properties-common && \
        add-apt-repository ppa:avsm/ppa -y && \
        add-apt-repository ppa:haxe/ocaml -y && \
        apt-get update -qqy && \
        apt-get install -qqy ocaml-nox camlp5 opam libpcre3-dev zlib1g-dev libgtk2.0-dev libmbedtls-dev ninja-build libstring-shellquote-perl libstring-shellquote-perl libipc-system-simple-perl curl
        
build:
    FROM +install-neko
    
    ARG ADD_REVISION
    ARG TARGETPLATFORM
    
    # Copy files
    COPY . .
    
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