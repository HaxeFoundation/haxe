package cpp.encoding;

import cpp.UInt8;
import cpp.Int64;
import cpp.Char32;
import cpp.marshal.View;
import haxe.extern.AsVar;

@:semantics(value)
@:cpp.PointerType({ namespace : [ "cpp", "encoding" ] })
extern class Utf16 {
    static function isEncoded(string:String):Bool;

    static overload function getByteCount(codepoint:Char32):Int64;
    static overload function getByteCount(string:String):Int64;

    static overload function encode(string:String, buffer:View<UInt8>):Int64;
    static overload function encode(codepoint:Char32, buffer:View<UInt8>):Int64;

    static overload function decode(buffer:View<UInt8>):String;
    static overload function decode(buffer:View<UInt8>, codepoint:AsVar<Char32>):Int64;
}