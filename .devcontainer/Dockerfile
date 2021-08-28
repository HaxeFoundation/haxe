FROM ubuntu:18.04

RUN apt-get update -qqy
RUN apt-get install -qqy sudo software-properties-common

RUN add-apt-repository ppa:avsm/ppa -y
RUN add-apt-repository ppa:haxe/ocaml -y
RUN apt-get update -qqy
RUN apt-get install -qqy ocaml-nox camlp5 opam libpcre3-dev zlib1g-dev libgtk2.0-dev libmbedtls-dev ninja-build libstring-shellquote-perl libipc-system-simple-perl neko neko-dev

RUN useradd -ms /bin/bash haxe && echo "haxe:haxe" | chpasswd && adduser haxe sudo
USER haxe

ENV OPAMYES=1
RUN opam init --disable-sandboxing
RUN opam update

# The following has to run with the cwd mounted to the container, see "postCreateCommand" in devcontainer.json
# RUN opam pin add haxe . --no-action
# RUN opam install haxe --deps-only --assume-depexts
