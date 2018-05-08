var s = String.fromCharCode(0xE9);
s == "é";
s.length == 1;
s.charCodeAt(0) == 0xE9;

var s = String.fromCharCode("あ".code);
s == "あ";
s.length == 1;
s.charCodeAt(0) == "あ".code;

var s = String.fromCharCode(0x1f602);
s == "😂";
#if (hl || js)
// UTF-16 surrogate pairs encoding
s.length == 2;
s.charCodeAt(0) == 55357;
s.charCodeAt(1) == 56834;
#else
s.length == 1;
s.charCodeAt(0) == "😂".code;
#end

var s = "é" + "あ";
s == "éあ";
s.length == 2;
s.charCodeAt(1) == "あ".code;

var s = "é" + "あ" + "😂";
s == "éあ😂";

var buf = new StringBuf();
buf.addChar(0xE9);
buf.addChar("あ".code);
buf.add("é");
buf.add("あ");
var str = buf.toString();
str.length == 4;
str == "éあéあ";
str.charCodeAt(3) == "あ".code;

var str = StringTools.urlEncode("éあ😂");
str == "%C3%A9%E3%81%82%F0%9F%98%82";
str = StringTools.urlDecode(str);
str == "éあ😂";

var str = haxe.Serializer.run("éあ");
str == "y15:%C3%A9%E3%81%82";
str = haxe.Unserializer.run(str);
str == "éあ";

var str = haxe.Serializer.run("😂");
str == "y12:%F0%9F%98%82";
str = haxe.Unserializer.run(str);
str == "😂";

var str = haxe.io.Bytes.ofString("éあ😂");
str.toHex() == "c3a9e38182f09f9882";

#if (hl || js)
var str = haxe.io.Bytes.ofString("éあ😂",RawNative);
str.toHex() == "e90042303dd802de"; // UCS2 native
#end
