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

haxe.Utf8.validate(haxe.io.Bytes.ofHex("f0a9b8bde38182c3ab61")) == true;
haxe.Utf8.validate(haxe.io.Bytes.ofHex("ed9fbf")) == true;
haxe.Utf8.validate(haxe.io.Bytes.ofHex("ee8080")) == true;
haxe.Utf8.validate(haxe.io.Bytes.ofHex("f48fbfbf")) == true;
haxe.Utf8.validate(haxe.io.Bytes.ofHex("f0a9b8bde381c3ab61")) == false;
haxe.Utf8.validate(haxe.io.Bytes.ofHex("c0af")) == false; // redundant sequence
haxe.Utf8.validate(haxe.io.Bytes.ofHex("eda080")) == false; // surrogate byte sequence
haxe.Utf8.validate(haxe.io.Bytes.ofHex("edbfbf")) == false; // surrogate byte sequence
haxe.Utf8.validate(haxe.io.Bytes.ofHex("f4908080")) == false; // U+110000