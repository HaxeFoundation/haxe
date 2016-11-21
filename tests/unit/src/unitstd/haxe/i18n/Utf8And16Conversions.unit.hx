var wrapUtf8 = function (s) return new haxe.i18n.Utf8(s);
var wrapUtf16 = function (s) return new haxe.i18n.Utf16(s);
var charUtf8 = function (i) return haxe.i18n.Utf8.fromCharCode(i);
var charUtf16 = function (i) return haxe.i18n.Utf16.fromCharCode(i);

var eqUtf16 = function (a:haxe.i18n.Utf16, b:haxe.i18n.Utf16, ?pos:haxe.PosInfos) {
	eqAbstract(a == b, a.toCodeArray(), b.toCodeArray(), pos);
}
var eqUtf8 = function (a:haxe.i18n.Utf8, b:haxe.i18n.Utf8, ?pos:haxe.PosInfos) {
	eqAbstract(a == b, a.toCodeArray(), b.toCodeArray(), pos);
}
// test utf16 to utf8 conversion

// 3 bytes utf-8


eqUtf8(
	wrapUtf8("\u{4E00}"), 
	wrapUtf16("\u{4E00}").toUtf8()
);

eqUtf8(
	wrapUtf8("\u{4E00}"), 
	wrapUtf16("\u{4E00}").toUtf8()
);


eqUtf8(
	wrapUtf16("€").toUtf8(), 
	wrapUtf8("€")
);

// 2 bytes utf-8
eqUtf8(
	wrapUtf16("ä").toUtf8(), 
	wrapUtf8("ä")
);

// 4 bytes utf-8
//"\u{1D11E}" // 𝄞

var violine = 0x1D11E;

eqUtf8(
	charUtf16(violine).toUtf8(), 
	charUtf8(violine)
);

// 1 byte utf-8
eqUtf8(
	wrapUtf16("a").toUtf8(), 
	wrapUtf8("a")
);


// check combination
eqUtf8(
	(wrapUtf16("1")+charUtf16(violine)+wrapUtf16("aä\u{4E00}€")).toUtf8(),
	wrapUtf8("1")+charUtf8(violine)+wrapUtf8("aä\u{4E00}€")
);



// test utf8 to utf16 conversion

// 3 bytes utf-8
eqUtf16(
	wrapUtf16("\u{4E00}"), 
	wrapUtf8("\u{4E00}").toUtf16()
);


eqUtf16(
	wrapUtf8("€").toUtf16(), 
	wrapUtf16("€")
);

// 2 bytes utf-8
eqUtf16(
	wrapUtf8("ä").toUtf16(), 
	wrapUtf16("ä")
);


// 4 bytes utf-8
eqUtf16(
	charUtf8(violine).toUtf16(), 
	charUtf16(violine)
);


// 1 byte utf-8
eqUtf16(
	wrapUtf8("a").toUtf16(), 
	wrapUtf16("a")
);


// check combination
eqUtf16(
	(wrapUtf8("1")+charUtf8(violine)+wrapUtf8("aä\u{4E00}€")).toUtf16(), 
	wrapUtf16("1")+charUtf16(violine)+wrapUtf16("aä\u{4E00}€")
);
