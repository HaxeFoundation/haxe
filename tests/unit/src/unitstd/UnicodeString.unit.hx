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
s.charAt( -1) == "";
("":UnicodeString).charAt(0) == "";
("":UnicodeString).charAt(1) == "";
("":UnicodeString).charAt( -1) == "";

// charCodeAt
s.charCodeAt(0) == codes[0];
s.charCodeAt(1) == codes[1];
s.charCodeAt(2) == codes[2];
s.charCodeAt(3) == null;
s.charCodeAt(-1) == null;

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
(s + s2).length == s.length + (s2:UnicodeString).length;
var s3 = s;
(s3 += s2).length == s.length + (s2:UnicodeString).length;

// iterator
aeq(codes, [for(c in s) c]);

// keyValueIterator
var keys = [for(i in 0...codes.length) i];
var actualKeyCodes = [for(i => c in s) [i, c]];
aeq(keys, actualKeyCodes.map(a -> a[0]));
aeq(codes, actualKeyCodes.map(a -> a[1]));

// validate
UnicodeString.validate(haxe.io.Bytes.ofHex("f0a9b8bde38182c3ab61"), UTF8) == true;
UnicodeString.validate(haxe.io.Bytes.ofHex("ed9fbf"), UTF8) == true;
UnicodeString.validate(haxe.io.Bytes.ofHex("ee8080"), UTF8) == true;
UnicodeString.validate(haxe.io.Bytes.ofHex("f48fbfbf"), UTF8) == true;
UnicodeString.validate(haxe.io.Bytes.ofHex("f0a9b8bde381c3ab61"), UTF8) == false;
UnicodeString.validate(haxe.io.Bytes.ofHex("c0af"), UTF8) == false; // redundant sequence
UnicodeString.validate(haxe.io.Bytes.ofHex("eda080"), UTF8) == false; // surrogate byte sequence
UnicodeString.validate(haxe.io.Bytes.ofHex("edbfbf"), UTF8) == false; // surrogate byte sequence
UnicodeString.validate(haxe.io.Bytes.ofHex("f4908080"), UTF8) == false; // U+110000