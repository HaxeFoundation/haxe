package haxe.crypto.padding;

@:enum
abstract Padding(String)
{
    var PKCS5 = "PKCS5";
    var NoPadding = "NoPadding";
    var BitPadding = "BitPadding";
    var AnsiX923 = "AnsiX923";
}
