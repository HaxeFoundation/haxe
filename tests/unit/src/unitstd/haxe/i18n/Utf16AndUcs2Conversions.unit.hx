var wrapUtf16 = function (s) return new haxe.i18n.Utf16(s);
var wrapUcs2 = function (s) return new haxe.i18n.Ucs2(s);
var charUtf16 = function (i) return haxe.i18n.Utf16.fromCharCode(i);
var charUcs2 = function (i) return haxe.i18n.Ucs2.fromCharCode(i);
// test Ucs2 to utf16 conversion

// 3 bytes utf-8


var x = wrapUcs2("\u{4E00}");


wrapUtf16("\u{4E00}") == wrapUcs2("\u{4E00}").toUtf16();

wrapUtf16("\u{4E00}") == wrapUcs2("\u{4E00}").toUtf16();


wrapUcs2("€").toUtf16() == wrapUtf16("€");

// 2 bytes utf-8
wrapUcs2("ä").toUtf16() == wrapUtf16("ä");

// 4 bytes utf-8
//"\u{1D11E}" // 𝄞

// 1 byte utf-8
wrapUcs2("a").toUtf16() == wrapUtf16("a");

// test utf16 to Ucs2 conversion

// 3 bytes utf-8
wrapUcs2("\u{4E00}") == wrapUtf16("\u{4E00}").toUcs2();


wrapUtf16("€").toUcs2() == wrapUcs2("€");

// 2 bytes utf-8
wrapUtf16("ä").toUcs2() == wrapUcs2("ä");




// 1 byte utf-8
wrapUtf16("a").toUcs2() == wrapUcs2("a");


