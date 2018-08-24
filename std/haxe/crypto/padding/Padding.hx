package haxe.crypto.padding;

@:enum
abstract Padding(String)
{
    var PKCS5 = "PKCS5";
    var NoPadding = "NoPadding";
    var BitPadding = "BitPadding";
    var AnsiX923 = "AnsiX923";
    var ISO10126 = "ISO10126";
    var NullPadding = "NullPadding";
    var SpacePadding = "SpacePadding";
    var TBC = "TBC";
}
