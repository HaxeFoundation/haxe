// new

var wrap = function (s) return new haxe.i18n.Ucs2(s);

var arrEq = function (a:Array<haxe.i18n.Ucs2>, b:Array<haxe.i18n.Ucs2>) {
	t(a.length == b.length);
	for (i in 0...a.length) {
		var a1 = a[i];
		var b1 = b[i];
		t(a1 == b1);
	}
}

var eq1 = function (a:haxe.i18n.Ucs2, b:haxe.i18n.Ucs2, ?pos:haxe.PosInfos) {
	eqAbstract(a == b, a.toCodeArray(), b.toCodeArray(), pos);
}

var violine = 0x1D11E; // 𝄞.code 

wrap("𝄞").length == 2;

eq1(wrap("𝄞"), haxe.i18n.Ucs2.fromCharCode(violine));  

// ucs2 strings can store surrogate pairs, but they count as separate characters


wrap("𝄞").charCodeAt(0) == 0xD834;
wrap("𝄞").charCodeAt(1) == 0xDD1E;

var x = haxe.i18n.Ucs2.fromCharCode(violine);
x.length == 2;
x.isValid() == false;

wrap("𝄞").isValid() == false;

// substr with special chars
var s = wrap("xऽॐÉxऽॐÉxxbarxbarxÉ");
eq1(s.substr(0), wrap("xऽॐÉxऽॐÉxxbarxbarxÉ"));
eq1(s.substr(1), wrap("ऽॐÉxऽॐÉxxbarxbarxÉ"));
eq1(s.substr(19),  wrap(""));
eq1(s.substr(18), wrap("É"));
eq1(s.substr(17), wrap("xÉ"));

eq1(s.substr(-1), wrap("É"));
eq1(s.substr(-2), wrap("xÉ"));
eq1(s.substr(-18), wrap("ऽॐÉxऽॐÉxxbarxbarxÉ"));
eq1(s.substr(-19), wrap("xऽॐÉxऽॐÉxxbarxbarxÉ"));
eq1(s.substr( -100), wrap("xऽॐÉxऽॐÉxxbarxbarxÉ"));

eq1(s.substr(0, 0), wrap(""));
eq1(s.substr(0, 1), wrap("x"));
eq1(s.substr(0, 2), wrap("xऽ"));

eq1(s.substr(0, 100), wrap("xऽॐÉxऽॐÉxxbarxbarxÉ"));

eq1(s.substr(0, -1), wrap("xऽॐÉxऽॐÉxxbarxbarx"));
eq1(s.substr(0, -2), wrap("xऽॐÉxऽॐÉxxbarxbar"));

eq1(s.substr(0, -100), wrap(""));

// substring with special chars
var s = wrap("ऽॐÉoxfooxxbarxbarxx");
eq1(s.substring(0, 0), wrap(""));
eq1(s.substring(0, 1), wrap("ऽ"));
eq1(s.substring(1, 0), wrap("ऽ"));
eq1(s.substring(0, 2), wrap("ऽॐ"));
eq1(s.substring(2, 0), wrap("ऽॐ"));
eq1(s.substring(-1, 0), wrap(""));
eq1(s.substring(0, -1), wrap(""));
eq1(s.substring(-1, -1), wrap(""));
eq1(s.substring(-1, 1), wrap("ऽ"));
eq1(s.substring(1, -1), wrap("ऽ"));
eq1(s.substring(-1, 2), wrap("ऽॐ"));
eq1(s.substring(2, -1), wrap("ऽॐ"));
eq1(s.substring(0), wrap("ऽॐÉoxfooxxbarxbarxx"));
eq1(s.substring(1), wrap("ॐÉoxfooxxbarxbarxx"));
eq1(s.substring(2), wrap("Éoxfooxxbarxbarxx"));
eq1(s.substring(0, -1), wrap(""));
eq1(s.substring(1, -1), wrap("ऽ"));
eq1(s.substring(2, -1), wrap("ऽॐ"));
eq1(s.substring(20, 0), wrap("ऽॐÉoxfooxxbarxbarxx"));
eq1(s.substring(0, 100), wrap("ऽॐÉoxfooxxbarxbarxx"));
eq1(s.substring(100, 120), wrap(""));
eq1(s.substring(100, 0), wrap("ऽॐÉoxfooxxbarxbarxx"));
eq1(s.substring(120, 100), wrap(""));

// split with special chars
var s = wrap("xॐÉxॐÉxxऽॐxऽॐxx");
arrEq(s.split(wrap("x")),[wrap(""), wrap("ॐÉ"), wrap("ॐÉ"), wrap(""), wrap("ऽॐ"), wrap("ऽॐ"), wrap(""),wrap("")]);

arrEq(s.split(wrap("xx")),[wrap("xॐÉxॐÉ"),wrap("ऽॐxऽॐ"),wrap("")]);


// lastIndexOf with special chars
var s = wrap("ऽooऽooऽoobॐrbॐr");

s.lastIndexOf(wrap("r")) == 14;

s.lastIndexOf(wrap("ॐ")) == 13;
s.lastIndexOf(wrap("b")) == 12;

s.lastIndexOf(wrap("bॐr")) == 12;
s.lastIndexOf(wrap("ऽoo")) == 6;
s.lastIndexOf(wrap("ऽooऽoo")) == 3;
s.lastIndexOf(wrap("ऽ")) == 6;
s.lastIndexOf(wrap("bॐrb")) == 9;
s.lastIndexOf(wrap("z")) == -1;

s.lastIndexOf(wrap("r"), 14) == 14;


s.lastIndexOf(wrap("r"), 13) == 11;

s.lastIndexOf(wrap("ॐ"), 14) == 13;

s.lastIndexOf(wrap("ॐ"), 13) == 13;
s.lastIndexOf(wrap("ॐ"), 12) == 10;

s.lastIndexOf(wrap("bॐr"), 12) == 12;

s.lastIndexOf(wrap("bॐr"), 11) == 9;
s.lastIndexOf(wrap("bॐr"), 9) == 9;
s.lastIndexOf(wrap("bॐr"), 8) == -1;

// charAt with special chars
eq1(wrap("fॐॐ1ऽar").charAt(0), wrap("f"));
eq1(wrap("fॐॐ1ऽar").charAt(1), wrap("ॐ"));
eq1(wrap("fॐॐ1ऽar").charAt(2), wrap("ॐ"));
eq1(wrap("fॐॐ1ऽar").charAt(3), wrap("1"));
eq1(wrap("fॐॐ1ऽar").charAt(4), wrap("ऽ"));
eq1(wrap("fॐॐ1ऽar").charAt(5), wrap("a"));
eq1(wrap("fॐॐ1ऽar").charAt(6), wrap("r"));
eq1(wrap("fॐॐ1ऽar").charAt(7), wrap(""));


// indexOf with special chars
var s = wrap("ॐऽऽ1bar");

s.indexOf(wrap("ॐ")) == 0;

s.indexOf(wrap("ऽ")) == 1;

s.indexOf(wrap("1")) == 3;
s.indexOf(wrap("b")) == 4;
s.indexOf(wrap("a")) == 5;
s.indexOf(wrap("r")) == 6;
s.indexOf(wrap("z")) == -1;

s.indexOf(wrap("ॐऽऽ")) == 0;
s.indexOf(wrap("ऽऽ")) == 1;

s.indexOf(wrap("ऽ"), 1) == 1;
s.indexOf(wrap("ऽ"), 2) == 2;
s.indexOf(wrap("ऽ"), 3) == -1;

eq1(wrap("ॐऽऽ").toUpperCase(), wrap("ॐऽऽ"));
eq1(wrap("ॐऽaऽ").toUpperCase(), wrap("ॐऽAऽ"));
