var wrapUtf8 = function (s) return new haxe.i18n.Utf8(s);
var wrapUcs2 = function (s) return new haxe.i18n.Ucs2(s);
var charUtf8 = function (i) return haxe.i18n.Utf8.fromCharCode(i);
var charUcs2 = function (i) return haxe.i18n.Ucs2.fromCharCode(i);
// test Ucs2 to utf8 conversion

var eqUcs2 = function (a:haxe.i18n.Ucs2, b:haxe.i18n.Ucs2, ?pos:haxe.PosInfos) {
	eqAbstract(a == b, a.toCodeArray(), b.toCodeArray(), pos);
}
var eqUtf8 = function (a:haxe.i18n.Utf8, b:haxe.i18n.Utf8, ?pos:haxe.PosInfos) {
	eqAbstract(a == b, a.toCodeArray(), b.toCodeArray(), pos);
}

// 3 bytes utf-8


var x = wrapUcs2("\u{4E00}");

eqUtf8(
	wrapUtf8("\u{4E00}"),
	wrapUcs2("\u{4E00}").toUtf8()
);
eqUtf8(
	wrapUtf8("\u{4E00}"),
	wrapUcs2("\u{4E00}").toUtf8()
);

eqUtf8(
	wrapUcs2("€").toUtf8(),
	wrapUtf8("€")
);

// 2 bytes utf-8
eqUtf8(
	wrapUcs2("ä").toUtf8(),
	wrapUtf8("ä")
);

// 1 byte utf-8
eqUtf8(
	wrapUcs2("a").toUtf8(),
	wrapUtf8("a")
);

// test utf8 to Ucs2 conversion

// 3 bytes utf-8
eqUcs2(
	wrapUcs2("\u{4E00}"),
	wrapUtf8("\u{4E00}").toUcs2()
);


eqUcs2(
	wrapUtf8("€").toUcs2(),
	wrapUcs2("€")
);

// 2 bytes utf-8
eqUcs2(
	wrapUtf8("ä").toUcs2(),
	wrapUcs2("ä")
);

// 1 byte utf-8
eqUcs2(
	wrapUtf8("a").toUcs2(),
	wrapUcs2("a")
);


