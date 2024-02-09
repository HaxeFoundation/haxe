#if target.unicode
var s = new UnicodeString("𠜎zя");
var codes = [132878, 122, 1103];

// length
s.length == codes.length;
// // toUpperCase, toLowerCase
// var turkishLower = "ğüşıiöç";
// var turkishUpper = "ĞÜŞIİÖÇ";
// turkishUpper == turkishLower.toUpperCase();
// turkishLower == turkishUpper.toLowerCase();
// charAt
s.charAt(0) == "𠜎";
s.charAt(1) == "z";
s.charAt(2) == "я";
s.charAt(3) == "";
s.charAt(-1) == "";
("" : UnicodeString).charAt(0) == "";
("" : UnicodeString).charAt(1) == "";
("" : UnicodeString).charAt(-1) == "";
// charCodeAt
s.charCodeAt(0) == codes[0];
s.charCodeAt(1) == codes[1];
s.charCodeAt(2) == codes[2];
s.charCodeAt(3) == null;
s.charCodeAt(-1) == null;
// indexOf
var s:UnicodeString = "𠜎zяяw";
s.indexOf("𠜎") == 0;
s.indexOf("z") == 1;
s.indexOf("я") == 2;
s.indexOf("zя") == 1;
s.indexOf("w") == 4;
s.indexOf("яw") == 3;
s.indexOf("f") == -1;
s.indexOf("я", 0) == 2;
s.indexOf("я", 1) == 2;
s.indexOf("я", 2) == 2;
s.indexOf("я", 3) == 3;
s.indexOf("я", 4) == -1;
s.indexOf("я", 40) == -1;
#if !lua // TODO https://github.com/HaxeFoundation/haxe/pull/8370
s.indexOf("я", -1) == 2;
s.indexOf("я", -2) == 2;
s.indexOf("я", -3) == 2;
s.indexOf("я", -4) == 2;
s.indexOf("я", -5) == 2;
s.indexOf("я", -50) == 2;
#end
// lastIndexOf
var s:UnicodeString = "𠜎zяяw";
s.lastIndexOf("𠜎") == 0;
s.lastIndexOf("z") == 1;
s.lastIndexOf("я") == 3;
s.lastIndexOf("zя") == 1;
s.lastIndexOf("яw") == 3;
s.lastIndexOf("f") == -1;
s.lastIndexOf("я", 0) == -1;
s.lastIndexOf("я", 1) == -1;
s.lastIndexOf("я", 2) == 2;
s.lastIndexOf("я", 3) == 3;
s.lastIndexOf("я", 4) == 3;
s.lastIndexOf("я", 40) == 3;
// substr
var s:UnicodeString = "𠜎zяяw";
s.substr(0) == "𠜎zяяw";
s.substr(1) == "zяяw";
s.substr(5) == "";
s.substr(4) == "w";
s.substr(3) == "яw";
s.substr(-1) == "w";
s.substr(-2) == "яw";
s.substr(-4) == "zяяw";
s.substr(-5) == "𠜎zяяw";
s.substr(-100) == "𠜎zяяw";
s.substr(0, 0) == "";
s.substr(0, 1) == "𠜎";
s.substr(0, 2) == "𠜎z";
s.substr(0, 100) == "𠜎zяяw";
s.substr(0, -1) == "𠜎zяя";
s.substr(0, -2) == "𠜎zя";
s.substr(0, -100) == "";
// substring
var s:UnicodeString = "𠜎zяяw";
s.substring(0, 0) == "";
s.substring(0, 1) == "𠜎";
s.substring(1, 0) == "𠜎";
s.substring(0, 2) == "𠜎z";
s.substring(2, 0) == "𠜎z";
s.substring(-1, 0) == "";
s.substring(0, -1) == "";
s.substring(-1, -1) == "";
s.substring(-1, 1) == "𠜎";
s.substring(1, -1) == "𠜎";
s.substring(-1, 2) == "𠜎z";
s.substring(2, -1) == "𠜎z";
s.substring(0) == "𠜎zяяw";
s.substring(1) == "zяяw";
s.substring(2) == "яяw";
s.substring(0, -1) == "";
s.substring(5, 0) == "𠜎zяяw";
s.substring(0, 100) == "𠜎zяяw";
s.substring(100, 120) == "";
s.substring(100, 0) == "𠜎zяяw";
s.substring(120, 100) == "";
s.substring(1, 4) == "zяя";
s.substring(4, 1) == "zяя";
var s = new UnicodeString("𠜎zя");

// @:op(UnicodeString)
var s2 = new UnicodeString("𠜎z");
s != s2;
!(s == s2);
s > s2;
s >= s2;
s2 < s;
s2 <= s;
(s + s2).length == s.length + s2.length;
var s3 = s;
(s3 += s2).length == s.length + s2.length;
// @:op(String)
var s2 = "abя";
s != s2;
!(s == s2);
s > s2;
s >= s2;
s2 < s;
s2 <= s;
(s + s2).length == s.length + (s2 : UnicodeString).length;
var s3 = s;
(s3 += s2).length == s.length + (s2 : UnicodeString).length;
// iterator
aeq(codes, [for (c in s) c]);
// keyValueIterator
var keys = [for (i in 0...codes.length) i];
var actualKeyCodes = [for (i => c in s) [i, c]];
aeq(keys, actualKeyCodes.map(a -> a[0]));
aeq(codes, actualKeyCodes.map(a -> a[1]));
// validate
UnicodeString.validate(haxe.io.Bytes.ofHex("f0a9b8bde38182c3ab61"), UTF8) == true;
UnicodeString.validate(haxe.io.Bytes.ofHex("ed9fbf"), UTF8) == true;
UnicodeString.validate(haxe.io.Bytes.ofHex("ee8080"), UTF8) == true;
UnicodeString.validate(haxe.io.Bytes.ofHex("f48fbfbf"), UTF8) == true;
UnicodeString.validate(haxe.io.Bytes.ofHex("f0a9b8bde381c3ab61"), UTF8) == false;
UnicodeString.validate(haxe.io.Bytes.ofHex("c0af"), UTF8) == false; // overlong sequence
UnicodeString.validate(haxe.io.Bytes.ofHex("eda080"), UTF8) == false; // surrogate byte sequence
UnicodeString.validate(haxe.io.Bytes.ofHex("edbfbf"), UTF8) == false; // surrogate byte sequence
UnicodeString.validate(haxe.io.Bytes.ofHex("f4908080"), UTF8) == false; // U+110000
#else
1 == 1;
#end
