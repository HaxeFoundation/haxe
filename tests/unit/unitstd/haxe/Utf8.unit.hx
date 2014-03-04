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
