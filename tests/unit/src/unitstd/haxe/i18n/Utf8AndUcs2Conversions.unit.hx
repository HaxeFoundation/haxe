var wrapUtf8 = function (s) return new haxe.i18n.Utf8(s);
var wrapUcs2 = function (s) return new haxe.i18n.Ucs2(s);
var charUtf8 = function (i) return haxe.i18n.Utf8.fromCharCode(i);
var charUcs2 = function (i) return haxe.i18n.Ucs2.fromCharCode(i);
// test Ucs2 to utf8 conversion

// 3 bytes utf-8


var x = wrapUcs2("\u{4E00}");


wrapUtf8("\u{4E00}") == wrapUcs2("\u{4E00}").toUtf8();

wrapUtf8("\u{4E00}") == wrapUcs2("\u{4E00}").toUtf8();


wrapUcs2("€").toUtf8() == wrapUtf8("€");

// 2 bytes utf-8
wrapUcs2("ä").toUtf8() == wrapUtf8("ä");

// 4 bytes utf-8
//"\u{1D11E}" // 𝄞

// 1 byte utf-8
wrapUcs2("a").toUtf8() == wrapUtf8("a");

// test utf8 to Ucs2 conversion

// 3 bytes utf-8
wrapUcs2("\u{4E00}") == wrapUtf8("\u{4E00}").toUcs2();


wrapUtf8("€").toUcs2() == wrapUcs2("€");

// 2 bytes utf-8
wrapUtf8("ä").toUcs2() == wrapUcs2("ä");




// 1 byte utf-8
wrapUtf8("a").toUcs2() == wrapUcs2("a");


