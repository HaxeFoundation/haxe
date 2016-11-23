// new

var wrap = function (s) return new haxe.i18n.Utf32(s);

var arrEq = function (a:Array<haxe.i18n.Utf32>, b:Array<haxe.i18n.Utf32>) {
	t(a.length == b.length);
	for (i in 0...a.length) {
		var a1 = a[i];
		var b1 = b[i];
		t(a1 == b1);
	}
}

var eq1 = function (a:haxe.i18n.Utf32, b:haxe.i18n.Utf32, ?pos:haxe.PosInfos) {
	eqAbstract(a == b, a.toCodeArray(), b.toCodeArray(), pos);
}

var violine = 0x1D11E; // 𝄞.code 

wrap("𝄞").charCodeAt(0) == violine;

wrap("𝄞").length == 1;
wrap("𝄞𝄞𝄞").length == 3;


arrEq(wrap("𝄞_𝄞_𝄞").split(wrap("_")), [wrap("𝄞"), wrap("𝄞"), wrap("𝄞")]);

wrap("𝄞_𝄞_𝄞").lastIndexOf(wrap("𝄞_𝄞")) == 2;
wrap("𝄞_𝄞_𝄞").indexOf(wrap("𝄞_𝄞")) == 0;
wrap("𝄞_𝄞_𝄞aa").lastIndexOf(wrap("𝄞_𝄞")) == 2;
t(wrap("𝄞a𝄞") < wrap("𝄞b𝄞"));
t(wrap("𝄞a𝄞") <= wrap("𝄞b𝄞"));
t(wrap("𝄞b𝄞") > wrap("𝄞a𝄞"));
t(wrap("𝄞b𝄞") >= wrap("𝄞a𝄞"));

eq1(wrap("𝄞b𝄞").substring(0, 2), wrap("𝄞b"));
eq1(wrap("𝄞b𝄞").substr(0, 2), wrap("𝄞b"));
eq1(wrap("𝄞b𝄞").substr(0, -1), wrap("𝄞b"));
eq1(wrap("𝄞b𝄞").substr(0, 0), wrap(""));

// 3 byte char => ऽ 
// 4 byte char => 𝄞
// 2 byte char => É
// 1 byte char => a

wrap("ऽ𝄞Éa").length == 4;

wrap("ऽ𝄞ÉaऽÉ𝄞ÉÉ𝄞ÉÉ𝄞ÉÉ").length == 15; 

wrap("ऽ𝄞ÉaऽÉ𝄞ÉÉ𝄞ÉÉ𝄞ÉÉ").indexOf(wrap("É𝄞ÉÉ")) == 5;

wrap("ऽ𝄞ÉaऽÉ𝄞ÉÉ𝄞ÉÉ𝄞ÉÉ").indexOf(wrap("É𝄞ÉÉ")) == 5;

wrap("ऽ𝄞ÉaऽÉ𝄞ÉÉ𝄞ÉÉ𝄞ÉÉ").lastIndexOf(wrap("É𝄞ÉÉ")) == 11;
wrap("ऽ𝄞ÉaऽÉ𝄞ÉÉ𝄞ÉÉ𝄞ÉÉ").lastIndexOf(wrap("É𝄞ÉÉ")) == 11;

eq1(wrap("ऽ𝄞ÉaऽÉ𝄞ÉÉ𝄞ÉÉ𝄞ÉÉ").substr(0, -1), wrap("ऽ𝄞ÉaऽÉ𝄞ÉÉ𝄞ÉÉ𝄞É"));

// substr with special chars
var s = wrap("xऽ𝄞Éxऽ𝄞ÉxxbarxbarxÉ");
eq1(s.substr(0), wrap("xऽ𝄞Éxऽ𝄞ÉxxbarxbarxÉ"));
eq1(s.substr(1), wrap("ऽ𝄞Éxऽ𝄞ÉxxbarxbarxÉ"));
eq1(s.substr(19),  wrap(""));
eq1(s.substr(18), wrap("É"));
eq1(s.substr(17), wrap("xÉ"));

eq1(s.substr(-1), wrap("É"));
eq1(s.substr(-2), wrap("xÉ"));
eq1(s.substr(-18), wrap("ऽ𝄞Éxऽ𝄞ÉxxbarxbarxÉ"));
eq1(s.substr(-19), wrap("xऽ𝄞Éxऽ𝄞ÉxxbarxbarxÉ"));
eq1(s.substr( -100), wrap("xऽ𝄞Éxऽ𝄞ÉxxbarxbarxÉ"));

eq1(s.substr(0, 0), wrap(""));
eq1(s.substr(0, 1), wrap("x"));
eq1(s.substr(0, 2), wrap("xऽ"));

eq1(s.substr(0, 100), wrap("xऽ𝄞Éxऽ𝄞ÉxxbarxbarxÉ"));

eq1(s.substr(0, -1), wrap("xऽ𝄞Éxऽ𝄞Éxxbarxbarx"));
eq1(s.substr(0, -2), wrap("xऽ𝄞Éxऽ𝄞Éxxbarxbar"));

eq1(s.substr(0, -100), wrap(""));

// substring with special chars
var s = wrap("ऽ𝄞Éoxfooxxbarxbarxx");
eq1(s.substring(0, 0), wrap(""));
eq1(s.substring(0, 1), wrap("ऽ"));
eq1(s.substring(1, 0), wrap("ऽ"));
eq1(s.substring(0, 2), wrap("ऽ𝄞"));
eq1(s.substring(2, 0), wrap("ऽ𝄞"));
eq1(s.substring(-1, 0), wrap(""));
eq1(s.substring(0, -1), wrap(""));
eq1(s.substring(-1, -1), wrap(""));
eq1(s.substring(-1, 1), wrap("ऽ"));
eq1(s.substring(1, -1), wrap("ऽ"));
eq1(s.substring(-1, 2), wrap("ऽ𝄞"));
eq1(s.substring(2, -1), wrap("ऽ𝄞"));
eq1(s.substring(0), wrap("ऽ𝄞Éoxfooxxbarxbarxx"));
eq1(s.substring(1), wrap("𝄞Éoxfooxxbarxbarxx"));
eq1(s.substring(2), wrap("Éoxfooxxbarxbarxx"));
eq1(s.substring(0, -1), wrap(""));
eq1(s.substring(1, -1), wrap("ऽ"));
eq1(s.substring(2, -1), wrap("ऽ𝄞"));
eq1(s.substring(20, 0), wrap("ऽ𝄞Éoxfooxxbarxbarxx"));
eq1(s.substring(0, 100), wrap("ऽ𝄞Éoxfooxxbarxbarxx"));
eq1(s.substring(100, 120), wrap(""));
eq1(s.substring(100, 0), wrap("ऽ𝄞Éoxfooxxbarxbarxx"));
eq1(s.substring(120, 100), wrap(""));


// split with special chars
var s = wrap("x𝄞Éx𝄞Éxxऽ𝄞xऽ𝄞xx");
arrEq(s.split(wrap("x")),[wrap(""), wrap("𝄞É"), wrap("𝄞É"), wrap(""), wrap("ऽ𝄞"), wrap("ऽ𝄞"), wrap(""),wrap("")]);

arrEq(s.split(wrap("xx")),[wrap("x𝄞Éx𝄞É"),wrap("ऽ𝄞xऽ𝄞"),wrap("")]);


// lastIndexOf with special chars
var s = wrap("ऽooऽooऽoob𝄞rb𝄞r");

s.lastIndexOf(wrap("r")) == 14;

s.lastIndexOf(wrap("𝄞")) == 13;
s.lastIndexOf(wrap("b")) == 12;

s.lastIndexOf(wrap("b𝄞r")) == 12;
s.lastIndexOf(wrap("ऽoo")) == 6;
s.lastIndexOf(wrap("ऽooऽoo")) == 3;
s.lastIndexOf(wrap("ऽ")) == 6;
s.lastIndexOf(wrap("b𝄞rb")) == 9;
s.lastIndexOf(wrap("z")) == -1;

s.lastIndexOf(wrap("r"), 14) == 14;


s.lastIndexOf(wrap("r"), 13) == 11;

s.lastIndexOf(wrap("𝄞"), 14) == 13;

s.lastIndexOf(wrap("𝄞"), 13) == 13;
s.lastIndexOf(wrap("𝄞"), 12) == 10;

s.lastIndexOf(wrap("b𝄞r"), 12) == 12;

s.lastIndexOf(wrap("b𝄞r"), 11) == 9;
s.lastIndexOf(wrap("b𝄞r"), 9) == 9;
s.lastIndexOf(wrap("b𝄞r"), 8) == -1;


// charAt with special chars
eq1(wrap("f𝄞𝄞1ऽar").charAt(0), wrap("f"));
eq1(wrap("f𝄞𝄞1ऽar").charAt(1), wrap("𝄞"));
eq1(wrap("f𝄞𝄞1ऽar").charAt(2), wrap("𝄞"));
eq1(wrap("f𝄞𝄞1ऽar").charAt(3), wrap("1"));
eq1(wrap("f𝄞𝄞1ऽar").charAt(4), wrap("ऽ"));
eq1(wrap("f𝄞𝄞1ऽar").charAt(5), wrap("a"));
eq1(wrap("f𝄞𝄞1ऽar").charAt(6), wrap("r"));
eq1(wrap("f𝄞𝄞1ऽar").charAt(7), wrap(""));


// indexOf with special chars
var s = wrap("𝄞ऽऽ1bar");

s.indexOf(wrap("𝄞")) == 0;

s.indexOf(wrap("ऽ")) == 1;

s.indexOf(wrap("1")) == 3;
s.indexOf(wrap("b")) == 4;
s.indexOf(wrap("a")) == 5;
s.indexOf(wrap("r")) == 6;
s.indexOf(wrap("z")) == -1;

s.indexOf(wrap("𝄞ऽऽ")) == 0;
s.indexOf(wrap("ऽऽ")) == 1;

s.indexOf(wrap("ऽ"), 1) == 1;
s.indexOf(wrap("ऽ"), 2) == 2;
s.indexOf(wrap("ऽ"), 3) == -1;

eq1(wrap("𝄞ऽऽ").toLowerCase(), wrap("𝄞ऽऽ"));
eq1(wrap("𝄞ऽAऽ").toLowerCase(), wrap("𝄞ऽaऽ"));

eq1(wrap("𝄞ऽऽ").toUpperCase(), wrap("𝄞ऽऽ"));
eq1(wrap("𝄞ऽaऽ").toUpperCase(), wrap("𝄞ऽAऽ"));