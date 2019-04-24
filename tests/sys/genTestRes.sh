#!/bin/bash

# Generates files and directories in test-res used for Unicode sys tests.
# The test vector printf'ed into data.bin, as well as the names in filenames()
# should correspond exactly to the sequences in UnicodeSequences.valid.

# https://stackoverflow.com/questions/4774054/reliable-way-for-a-bash-script-to-get-the-full-path-to-itself
SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
cd "$SCRIPTPATH"

# delete previous, if any
[[ -e test-res ]] && rm -rf test-res

mkdir -p test-res

# generate files with Unicode data
printf "\x01\n"\
"\x7F\n"\
"\xC2\x80\n"\
"\xDF\xBF\n"\
"\xE0\xA0\x80\n"\
"\xED\x9F\xBF\n"\
"\xEE\x80\x80\n"\
"\xEF\xBF\xBD\n"\
"\xF0\x9F\x98\x82\xF0\x9F\x98\x84\xF0\x9F\x98\x99\n"\
"\xC8\xA7\n" > "test-res/data.bin"

function filenames() { # run a command or function with all test patterns
    # codepoint and UTF-8 bytes listed below
    # https://r12a.github.io/app-conversion/

    # boundary conditions

    # U+0001  01
    $1 `printf "\x01"`

    # U+007F  7F
    $1 `printf "\x7F"`

    # U+0080  C2 80
    $1 `printf "\xC2\x80"`

    # U+07FF  DF BF
    $1 `printf "\xDF\xBF"`

    # U+0800  E0 A0 80
    $1 `printf "\xE0\xA0\x80"`

    # U+D7FF  ED 9F BF
    $1 `printf "\xED\x9F\xBF"`

    # U+E000  EE 80 80
    $1 `printf "\xEE\x80\x80"`

    # U+FFFD  EF BF BD
    $1 `printf "\xEF\xBF\xBD"`

    # these are actually invalid
    # U+FFFE  EF BF BE
    # U+FFFF  EF BF BF
    #$1 `printf "\xEF\xBF\xBE"`
    #$1 `printf "\xEF\xBF\xBF"`

    # non-BMP (disabled for the time being)

    # U+10000 F0 90 80 80
    #$1 `printf "\xF0\x90\x80\x80"`

    # U+1FFFF F0 9F BF BF
    #$1 `printf "\xF0\x9F\xBF\xBF"`

    # U+FFFFF F3 BF BF BF
    #$1 `printf "\xF3\xBF\xBF\xBF"`

    # U+100000 F4 80 80 80
    #$1 `printf "\xF4\x80\x80\x80"`

    # U+10FFFF F4 8F BF BF
    #$1 `printf "\xF4\x8F\xBF\xBF"`

    # NFC / NFD

    # U+0227  C8 A7 (NFC)
    # on HFS+ will generate U+0061 U+0307 61 CC 87 instead (NFD)
    $1 `printf "\xC8\xA7"`

    # U+1F602 F0 9F 98 82
    # U+1F604 F0 9F 98 84
    # U+1F619 F0 9F 98 99
    $1 `printf "\xF0\x9F\x98\x82\xF0\x9F\x98\x84\xF0\x9F\x98\x99"`
}

function genDirs() { # fill a directory with test subdirectories
    pushd "$1"
    filenames "mkdir"
    popd
}

function genFiles() { # fill a directory with test files
    pushd "$1"
    filenames "cp $SCRIPTPATH/test-res/data.bin "
    popd
}

genDirs test-res

mkdir -p test-res/a # for nested directories
mkdir -p test-res/b # for files

pushd test-res
genDirs a
genFiles b
popd

function symLinkFill() { # symlinks used to test programPath and fullPath
    pushd "test-res/$1"
    ln -s ../../bin/cpp/UtilityProcess-debug bin-cpp-debug
    ln -s ../../bin/cpp/UtilityProcess bin-cpp
    ln -s ../../bin/cs/bin/UtilityProcess-Debug.exe bin-cs-debug
    ln -s ../../bin/cs/bin/UtilityProcess.exe bin-cs
    ln -s ../../bin/hl/UtilityProcess.hl bin-hl
    ln -s ../../bin/java/UtilityProcess-Debug.jar bin-java-debug
    ln -s ../../bin/java/UtilityProcess.jar bin-java
    ln -s ../../bin/neko/UtilityProcess.n bin-neko
    ln -s ../../bin/php/UtilityProcess/index.php bin-php
    ln -s ../../bin/python/UtilityProcess.py bin-py
    ln -s ../../src/UtilityProcess.hx bin-eval
    popd
}

filenames symLinkFill
