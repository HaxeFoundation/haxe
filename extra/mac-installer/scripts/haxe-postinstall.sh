#!/bin/sh
rm -f /usr/bin/haxe
rm -f /usr/local/bin/haxe
rm -f /usr/bin/haxedoc
rm -f /usr/local/bin/haxedoc
rm -f /usr/bin/haxelib
rm -f /usr/local/bin/haxelib
rm -f ~/.haxelib
rm -f $HOME/.haxelib 
ln -s /usr/local/lib/haxe/haxe /usr/local/bin/haxe
cp /usr/local/lib/haxe/haxelib /usr/local/bin/haxelib
mkdir -p /usr/local/lib/haxe/lib
chmod 777 /usr/local/lib/haxe/lib
