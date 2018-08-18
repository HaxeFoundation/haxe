package haxe.crypto.mode;

@:enum
abstract Mode(String)
{
    var CBC = "cbc";
    var CFB = "cfb";
    var CTR = "ctr";
    var ECB = "ecb";
    var OFB = "ofb";
    var PCBC = "pcbc";
}
