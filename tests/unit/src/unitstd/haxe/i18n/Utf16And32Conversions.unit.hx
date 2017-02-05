var wrapUtf16 = function (s) return new haxe.i18n.Utf16(s);
var wrapUtf32 = function (s) return new haxe.i18n.Utf32(s);
var charUtf16 = function (i) return haxe.i18n.Utf16.fromCharCode(i);
var charUtf32 = function (i) return haxe.i18n.Utf32.fromCharCode(i);

var eqUtf32 = function (a:haxe.i18n.Utf32, b:haxe.i18n.Utf32, ?pos:haxe.PosInfos) {
	eqAbstract(a == b, a.toCodeArray(), b.toCodeArray(), pos);
}
var eqUtf16 = function (a:haxe.i18n.Utf16, b:haxe.i18n.Utf16, ?pos:haxe.PosInfos) {
	eqAbstract(a == b, a.toCodeArray(), b.toCodeArray(), pos);
}
// test utf32 to utf16 conversion

// 3 bytes utf-8

eqUtf16(
	wrapUtf16("\u{4E00}"), 
	wrapUtf32("\u{4E00}").toUtf16()
);

eqUtf16(
	wrapUtf16("\u{4E00}"), 
	wrapUtf32("\u{4E00}").toUtf16()
);


eqUtf16(
	wrapUtf32("€").toUtf16(), 
	wrapUtf16("€")
);

// 2 bytes utf-8
eqUtf16(
	wrapUtf32("ä").toUtf16(), 
	wrapUtf16("ä")
);

// 4 bytes utf-8
//"\u{1D11E}" // 𝄞

var violine = 0x1D11E;

eqUtf16(
	charUtf32(violine).toUtf16(), 
	charUtf16(violine)
);

// 1 byte utf-8
eqUtf16(
	wrapUtf32("a").toUtf16(), 
	wrapUtf16("a")
);


// check combination
eqUtf16(
	(wrapUtf32("1")+charUtf32(violine)+wrapUtf32("aä\u{4E00}€")).toUtf16(),
	wrapUtf16("1")+charUtf16(violine)+wrapUtf16("aä\u{4E00}€")
);



// test utf16 to utf32 conversion

// 3 bytes utf-8
eqUtf32(
	wrapUtf32("\u{4E00}"), 
	wrapUtf16("\u{4E00}").toUtf32()
);


eqUtf32(
	wrapUtf16("€").toUtf32(), 
	wrapUtf32("€")
);

// 2 bytes utf-8
eqUtf32(
	wrapUtf16("ä").toUtf32(), 
	wrapUtf32("ä")
);


// 4 bytes utf-8
eqUtf32(
	charUtf16(violine).toUtf32(), 
	charUtf32(violine)
);


// 1 byte utf-8
eqUtf32(
	wrapUtf16("a").toUtf32(), 
	wrapUtf32("a")
);


// check combination
eqUtf32(
	(wrapUtf16("1")+charUtf16(violine)+wrapUtf16("aä\u{4E00}€")).toUtf32(), 
	wrapUtf32("1")+charUtf32(violine)+wrapUtf32("aä\u{4E00}€")
);
