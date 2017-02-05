var wrapUtf8 = function (s) return new haxe.i18n.Utf8(s);
var wrapUtf32 = function (s) return new haxe.i18n.Utf32(s);
var charUtf8 = function (i) return haxe.i18n.Utf8.fromCharCode(i);
var charUtf32 = function (i) return haxe.i18n.Utf32.fromCharCode(i);

var eqUtf32 = function (a:haxe.i18n.Utf32, b:haxe.i18n.Utf32, ?pos:haxe.PosInfos) {
	eqAbstract(a == b, a.toCodeArray(), b.toCodeArray(), pos);
}
var eqUtf8 = function (a:haxe.i18n.Utf8, b:haxe.i18n.Utf8, ?pos:haxe.PosInfos) {
	eqAbstract(a == b, a.toCodeArray(), b.toCodeArray(), pos);
}
// test utf32 to utf8 conversion

// 3 bytes utf-8

eqUtf8(
	wrapUtf8("\u{4E00}"), 
	wrapUtf32("\u{4E00}").toUtf8()
);

eqUtf8(
	wrapUtf8("\u{4E00}"), 
	wrapUtf32("\u{4E00}").toUtf8()
);


eqUtf8(
	wrapUtf32("€").toUtf8(), 
	wrapUtf8("€")
);

// 2 bytes utf-8
eqUtf8(
	wrapUtf32("ä").toUtf8(), 
	wrapUtf8("ä")
);

// 4 bytes utf-8
//"\u{1D11E}" // 𝄞

var violine = 0x1D11E;

eqUtf8(
	charUtf32(violine).toUtf8(), 
	charUtf8(violine)
);

// 1 byte utf-8
eqUtf8(
	wrapUtf32("a").toUtf8(), 
	wrapUtf8("a")
);


// check combination
eqUtf8(
	(wrapUtf32("1")+charUtf32(violine)+wrapUtf32("aä\u{4E00}€")).toUtf8(),
	wrapUtf8("1")+charUtf8(violine)+wrapUtf8("aä\u{4E00}€")
);



// test utf8 to utf32 conversion

// 3 bytes utf-8
eqUtf32(
	wrapUtf32("\u{4E00}"), 
	wrapUtf8("\u{4E00}").toUtf32()
);


eqUtf32(
	wrapUtf8("€").toUtf32(), 
	wrapUtf32("€")
);

// 2 bytes utf-8
eqUtf32(
	wrapUtf8("ä").toUtf32(), 
	wrapUtf32("ä")
);


// 4 bytes utf-8
eqUtf32(
	charUtf8(violine).toUtf32(), 
	charUtf32(violine)
);


// 1 byte utf-8
eqUtf32(
	wrapUtf8("a").toUtf32(), 
	wrapUtf32("a")
);


// check combination
eqUtf32(
	(wrapUtf8("1")+charUtf8(violine)+wrapUtf8("aä\u{4E00}€")).toUtf32(), 
	wrapUtf32("1")+charUtf32(violine)+wrapUtf32("aä\u{4E00}€")
);
