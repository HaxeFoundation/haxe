#if php
// php's haxe.Utf8 uses mbstring
if (untyped __call__("extension_loaded", "mbstring")) {
#end

#if false
// disabled tests with outside BMP chars (will be reenabled when we support them)
var str = "あ𠀀い";
haxe.Utf8.length(str) == 3;
haxe.Utf8.charCodeAt(str, 0) == 0x3042;
haxe.Utf8.charCodeAt(str, 1) == 0x20000;
haxe.Utf8.charCodeAt(str, 2) == 0x3044;
var buf = new haxe.Utf8();
buf.addChar(0x3042);
buf.addChar(0x20000);
buf.addChar(0x3044);
buf.toString() == str;
haxe.Utf8.compare(haxe.Utf8.sub(str, 0, 3), str) == 0;
haxe.Utf8.compare(haxe.Utf8.sub(str, 0, 2), "あ𠀀") == 0;
haxe.Utf8.compare(haxe.Utf8.sub(str, 1, 2), "𠀀い") == 0;
haxe.Utf8.compare(haxe.Utf8.sub(str, 0, 0), "") == 0;
haxe.Utf8.compare(haxe.Utf8.sub(str, 1, 0), "") == 0;
haxe.Utf8.compare(haxe.Utf8.sub(str, 9, 0), "") == 0;
#end


// same tests with BMP chars (actually UCS2 compliance only)
var str = "あéい";
haxe.Utf8.length(str) == 3;
haxe.Utf8.charCodeAt(str, 0) == 0x3042;
haxe.Utf8.charCodeAt(str, 1) == 0xE9;
haxe.Utf8.charCodeAt(str, 2) == 0x3044;
var big = new haxe.Utf8(10);
big.toString().length == 0;
var buf = new haxe.Utf8();
buf.addChar(0x3042);
buf.addChar(0xE9);
buf.addChar(0x3044);
buf.toString() == str;
haxe.Utf8.compare(haxe.Utf8.sub(str, 0, 3), str) == 0;
haxe.Utf8.compare(haxe.Utf8.sub(str, 0, 2), "あé") == 0;
haxe.Utf8.compare(haxe.Utf8.sub(str, 1, 2), "éい") == 0;
haxe.Utf8.compare(haxe.Utf8.sub(str, 0, 0), "") == 0;
haxe.Utf8.compare(haxe.Utf8.sub(str, 1, 0), "") == 0;

// unspecify outside of range Utf8.sub
// haxe.Utf8.compare(haxe.Utf8.sub(str, 9, 0), "") == 0;

// #if (neko || php || cpp || lua || macro)
// TODO neko, cpp, macro
#if (php || lua)
haxe.Utf8.validate("\xf0\xa9\xb8\xbd\xe3\x81\x82\xc3\xab\x61") == true;
haxe.Utf8.validate("\xed\x9f\xbf") == true;
haxe.Utf8.validate("\xee\x80\x80") == true;
haxe.Utf8.validate("\xf4\x8f\xbf\xbf") == true;
haxe.Utf8.validate("\xf0\xa9\xb8\xbd\xe3\x81\xc3\xab\x61") == false;
haxe.Utf8.validate("\xc0\xaf") == false; // redundant sequence
haxe.Utf8.validate("\xed\xa0\x80") == false; // surrogate byte sequence
haxe.Utf8.validate("\xed\xbf\xbf") == false; // surrogate byte sequence
haxe.Utf8.validate("\xf4\x90\x80\x80") == false; // U+110000
#end

#if php
}
#end
