#if !(neko || eval || (cpp && !cppia && !hxcpp_smart_strings)) // these platforms will not be made unicode-compatible


var s = String.fromCharCode(0xE9);
s == "é";
s.length == 1;
s.charCodeAt(0) == 0xE9;

var s = String.fromCharCode("あ".code);
s == "あ";
s.length == 1;
s.charCodeAt(0) == "あ".code;

var s = "aa😂éé";
s.indexOf(String.fromCharCode(0x80))<0;
s.indexOf("é")==s.length-2;
s.indexOf("aa")==0;
s.indexOf("a")==0;
s.lastIndexOf("a")==1;
s.indexOf("😂")>0;
s.lastIndexOf("😂")>0;
s.lastIndexOf("é")==s.length-1;
var s = "abc";
s.indexOf("éé")<0;
s.lastIndexOf("éé")<0;

var s = String.fromCharCode(0x1f602);
s == "😂";


#if php
// native UTF-16 or 32
s.length == 1;
s.charCodeAt(0) == "😂".code;
#else
// UTF-16 surrogate pairs encoding
s.length == 2;
s.charCodeAt(0) == 55357;
s.charCodeAt(1) == 56834;
#end

var s = "é" + "あ";
s == "éあ";
s.length == 2;
s.charCodeAt(1) == "あ".code;

var s = "é" + "😂" + "あ";
s == "é😂あ";
var a = s.split('😂');
a.length == 2;
a[0] == "é";
a[1] == "あ";

var a = s.split('');
#if php
// native UTF-16 or 32
a.length == 3;
a[0] == "é";
a[1] == "😂";
a[2] == "あ";
#else
a.length == 4;
a[0] == "é";
a[3] == "あ";
#end

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

var bytes = haxe.io.Bytes.ofString("éあ😂",RawNative);

#if (cpp || php)
bytes.toHex() == "c3a9e38182f09f9882"; // UTF-8 native
#else
bytes.toHex() == "e90042303dd802de"; // UTF-16 native
#end

bytes.getString(0,bytes.length,RawNative) == "éあ😂";

haxe.crypto.Md5.encode("éあ😂") == "d30b209e81e40d03dd474b26b77a8a18";
haxe.crypto.Sha1.encode("éあ😂") == "ec79856a75c98572210430aeb7fe6300b6c4e20c";
#if php //utf-8
haxe.crypto.Sha224.encode("éあ😂") == "d7967c5f27bd6868e276647583c55ab09d5f45b40610a3d9c6d91b90";
haxe.crypto.Sha256.encode("éあ😂") == "d0230b8d8ac2d6d0dbcee11ad0e0eaa68a6565347261871dc241571cab591676";
#else //utf-16
haxe.crypto.Sha224.encode("éあ😂") == "5132a98e08a503350384c765388a1a3b8b0b532f038eca94c881537e";
haxe.crypto.Sha256.encode("éあ😂") == "e662834bdc1a099b9f7b8d97975a1b1d9b6730c991268bba0e7fe7427e68be74";
#end
haxe.crypto.BaseCode.encode("éあ😂","0123456789abcdef") == "c3a9e38182f09f9882";

var buf = new haxe.io.BytesBuffer();
buf.addString("éあ😂");
buf.addString("éあ😂",RawNative);
var bytes = buf.getBytes();
bytes.getString(0,9) == "éあ😂";
bytes.getString(2,3) == "あ";
bytes.getString(5,4) == "😂";
bytes.getString(2,7) == "あ😂";
bytes.getString(9,bytes.length - 9,RawNative) == "éあ😂";

var o = new haxe.io.BytesOutput();
o.writeString("éあ😂");
o.writeString("éあ😂",RawNative);
var bytes2 = o.getBytes();
bytes2.toHex() == bytes.toHex();

var input = new haxe.io.BytesInput(bytes2);
input.readString(2) == "é";
input.readString(7) == "あ😂";
input.readString(bytes.length - 9,RawNative) == "éあ😂";


#end
