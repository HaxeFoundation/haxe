package cpp.encoding;

import cpp.UInt8;
import cpp.Int64;
import cpp.marshal.View;

@:semantics(value)
@:cpp.PointerType({ namespace : [ "cpp", "encoding" ] })
extern class Ascii {
    static function isEncoded(string:String):Bool;

    static function encode(string:String, buffer:View<UInt8>):Int64;

    static function decode(buffer:View<UInt8>):String;
}