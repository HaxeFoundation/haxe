/*
 // disable until we decide how to handle JS/SWF API being UCS2 and not UTF8
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
*/