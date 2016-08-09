
var wrapUtf8 = function (s) return new haxe.i18n.Utf8(s);
var wrapUtf16 = function (s) return new haxe.i18n.Utf16(s);

// 3 bytes utf-8
wrapUtf8("\u{4E00}") == wrapUtf16("\u{4E00}").toUtf8();
wrapUtf16("€").toUtf8() == wrapUtf8("€");

// 2 bytes utf-8
wrapUtf16("ä").toUtf8() == wrapUtf8("ä");

// 4 bytes utf-8
wrapUtf16("𝄞").toUtf8() == wrapUtf8("𝄞");

// 1 byte utf-8
wrapUtf16("abc").toUtf8() == wrapUtf8("abc");

