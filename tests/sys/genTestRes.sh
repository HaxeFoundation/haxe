#!/bin/bash

# Generates files and directories in test-res used for Unicode sys tests.

# https://stackoverflow.com/questions/4774054/reliable-way-for-a-bash-script-to-get-the-full-path-to-itself
SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
cd "$SCRIPTPATH"

mkdir -p test-res

# generate files with Unicode data
# disabled non-BMP: \n\xF0\x9F\xBF\xBF\n\xF3\xBF\xBF\xBF\n\xF4\x80\x80\x80\n\xF4\x8F\xBF\xBF
printf "\x01\n"\
"\x7F\n"\
"\xC2\x80\n"\
"\xDF\xBF\n"\
"\xE0\xA0\x80\n"\
"\xED\x9F\xBF\n"\
"\xEE\x80\x80\n"\
"\xEF\xBF\xBD\n"\
"\xC8\xA7\n"\
"\x61\n"\
"\x62\n"\
"\x63\n"\
"\xF0\x9F\x98\x82\xF0\x9F\x98\x84\xF0\x9F\x98\x99" > "test-res/data.bin"

function gen() { # fill a directory with files or directories
    GEN_CMD="cp $SCRIPTPATH/test-res/data.bin "
    if [[ "$1" == "dirs" ]]; then
        GEN_CMD="mkdir"
    fi
    pushd "$2"
    
    # codepoint and UTF-8 bytes listed below
    # https://r12a.github.io/app-conversion/
    
    # boundary conditions
    
    # U+0001  01
    $GEN_CMD `printf "\x01"`
    
    # U+007F  7F
    $GEN_CMD `printf "\x7F"`
    
    # U+0080  C2 80
    $GEN_CMD `printf "\xC2\x80"`
    
    # U+07FF  DF BF
    $GEN_CMD `printf "\xDF\xBF"`
    
    # U+0800  E0 A0 80
    $GEN_CMD `printf "\xE0\xA0\x80"`
    
    # U+D7FF  ED 9F BF
    $GEN_CMD `printf "\xED\x9F\xBF"`
    
    # U+E000  EE 80 80
    $GEN_CMD `printf "\xEE\x80\x80"`
    
    # U+FFFD  EF BF BD
    $GEN_CMD `printf "\xEF\xBF\xBD"`
    
    # these are actually invalid
    # U+FFFE  EF BF BE
    # U+FFFF  EF BF BF
    #$GEN_CMD `printf "\xEF\xBF\xBE"`
    #$GEN_CMD `printf "\xEF\xBF\xBF"`
    
    # non-BMP (disabled for the time being)
    
    # U+10000 F0 90 80 80
    #$GEN_CMD `printf "\xF0\x90\x80\x80"`
    
    # U+1FFFF F0 9F BF BF
    #$GEN_CMD `printf "\xF0\x9F\xBF\xBF"`
    
    # U+FFFFF F3 BF BF BF
    #$GEN_CMD `printf "\xF3\xBF\xBF\xBF"`
    
    # U+100000 F4 80 80 80
    #$GEN_CMD `printf "\xF4\x80\x80\x80"`
    
    # U+10FFFF F4 8F BF BF
    #$GEN_CMD `printf "\xF4\x8F\xBF\xBF"`
    
    # NFC / NFD
    
    # U+0227  C8 A7 (NFC)
    # on HFS+ will generate U+0061 U+0307 61 CC 87 instead (NFD)
    $GEN_CMD `printf "\xC8\xA7"`
    
    # some readable comparison cases
    
    # U+0061  61
    $GEN_CMD `printf "\x61"`
    
    # U+0062  62
    $GEN_CMD `printf "\x62"`
    
    # U+0063  63
    $GEN_CMD `printf "\x63"`
    
    # U+1F602 F0 9F 98 82
    # U+1F604 F0 9F 98 84
    # U+1F619 F0 9F 98 99
    $GEN_CMD `printf "\xF0\x9F\x98\x82\xF0\x9F\x98\x84\xF0\x9F\x98\x99"`
    
    popd
}

gen dirs test-res

pushd test-res
gen dirs a
gen files b
popd

# symlinks used to test programPath only
pushd test-res/c
ln -s ../bin/cpp/Main-debug bin-cpp
ln -s ../bin/cs/bin/Main-Debug.exe bin-cs
ln -s ../bin/hl/sys.hl bin-hl
ln -s ../bin/java/Main-Debug.jar bin-java
ln -s ../bin/neko/sys.n bin-neko
ln -s ../bin/php/Main bin-php
ln -s ../bin/python/sys.p bin-py
popd
