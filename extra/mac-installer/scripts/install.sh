#!/bin/sh
cd $(dirname $0)
# haxe
chmod +x *
./haxe-preinstall.sh
rm -f /usr/lib/haxe
rm -f /usr/local/lib/haxe
mkdir -p /usr/local/lib/haxe
mkdir -p /usr/local/bin
cp -Rf ../haxe/* /usr/local/lib/haxe
./haxe-postinstall.sh
./neko-preinstall.sh
rm -f /usr/local/lib/neko
mkdir -p /usr/local/lib/neko
cp -Rf ../neko/* /usr/local/lib/neko
./neko-postinstall.sh
cd ../
rm -Rf /tmp/haxe
