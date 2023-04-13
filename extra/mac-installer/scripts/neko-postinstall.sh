#!/bin/sh
rm -f /usr/bin/neko
rm -f /usr/bin/nekoc
rm -f /usr/bin/nekoml
rm -f /usr/bin/nekotools
rm -f /usr/lib/libneko.dylib
rm -f /usr/local/bin/neko
rm -f /usr/local/bin/nekoc
rm -f /usr/local/bin/nekoml
rm -f /usr/local/bin/nekotools
rm -f /usr/local/lib/libneko*.dylib

ln -s /usr/local/lib/neko/neko /usr/local/bin/neko
ln -s /usr/local/lib/neko/nekoc /usr/local/bin/nekoc
ln -s /usr/local/lib/neko/nekoml /usr/local/bin/nekoml
ln -s /usr/local/lib/neko/nekotools /usr/local/bin/nekotools
ln -s /usr/local/lib/neko/libneko.dylib /usr/local/lib/libneko.dylib
ln -s /usr/local/lib/neko/libneko.%%NEKO_MAJOR_VERSION%%.dylib /usr/local/lib/libneko.%%NEKO_MAJOR_VERSION%%.dylib
ln -s /usr/local/lib/neko/libneko.%%NEKO_VERSION%%.dylib /usr/local/lib/libneko.%%NEKO_VERSION%%.dylib
