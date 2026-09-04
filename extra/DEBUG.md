Run
```
make haxe.debug
```

Set the following environment variables before calling `ocamldebug`.
```
export LD_PRELOAD=$(pkg-config --variable=libdir libuv)/libuv.so
export HAXE_STD_PATH=/path/to/haxe/std
export CAML_LD_LIBRARY_PATH=/path/to/haxe/_build/default/libs/pcre2:/path/to/haxe/_build/default/libs/mbedtls:/path/to/haxe/_build/default/libs/extc:/path/to/haxe/_build/default/libs/extc:/path/to/haxe/_build/default/libs/objsize:$CAML_LD_LIBRARY_PATH
```

If the last one doesn't take, add it as a prefix to `ocamldebug`.
Add the following commands to your `.ocamldebug`, or run them first when starting it.
```
directory /path/to/haxe/_build/default/
directory /path/to/haxe/_build/default/src/.haxe.eobjs/byte
directory /path/to/haxe/_build/default/src/.haxe.objs/byte
directory /path/to/haxe/_build/default/libs/extc/.extc.objs/byte
directory /path/to/haxe/_build/default/libs/extc/.extproc.objs/byte
directory /path/to/haxe/_build/default/libs/extlib-leftovers/.extlib_leftovers.objs/byte
directory /path/to/haxe/_build/default/libs/ilib/.ilib.objs/byte
directory /path/to/haxe/_build/default/libs/javalib/.javalib.objs/byte
directory /path/to/haxe/_build/default/libs/json/.json.objs/byte
directory /path/to/haxe/_build/default/libs/mbedtls/.mbedtls.objs/byte
directory /path/to/haxe/_build/default/libs/neko/.neko.objs/byte
directory /path/to/haxe/_build/default/libs/objsize/.objsize.objs/byte
directory /path/to/haxe/_build/default/libs/pcre2/.pcre2.objs/byte
directory /path/to/haxe/_build/default/libs/swflib/.swflib.objs/byte
directory /path/to/haxe/_build/default/libs/ttflib/.ttflib.objs/byte
directory /path/to/haxe/_build/default/libs/ziplib/.ziplib.objs/byte
```


Tips:
On Unix use `rlwrap ocamldebug /path/to/haxe.debug <base.hxml>` for sensible QOL.
