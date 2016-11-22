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

eq1(wrap("foo"), wrap("foo"));
t(wrap("foo") == wrap("foo"));
t(!(wrap("foo") != wrap("foo")));


//trace(wrap("foo"));
//trace(wrap("foo").toLowerCase());
//trace(wrap("FOO"));

eq1(wrap("foo"), wrap("foo"));

// toUpperCase
eq1(wrap("foo").toUpperCase(), wrap("FOO"));
eq1(wrap("_bar").toUpperCase(), wrap("_BAR"));
eq1(wrap("123b").toUpperCase(), wrap("123B"));
eq1(wrap("").toUpperCase(), wrap(""));
eq1(wrap("A").toUpperCase(), wrap("A"));

// toLowerCase
eq1(wrap("FOO").toLowerCase(), wrap("foo"));
eq1(wrap("_BAR").toLowerCase(), wrap("_bar"));
eq1(wrap("123B").toLowerCase(), wrap("123b"));
eq1(wrap("").toLowerCase(), wrap(""));
eq1(wrap("a").toLowerCase(), wrap("a"));








// charAt
eq1(wrap("foo1bar").charAt(0), wrap("f"));
eq1(wrap("foo1bar").charAt(1), wrap("o"));
eq1(wrap("foo1bar").charAt(2), wrap("o"));
eq1(wrap("foo1bar").charAt(3), wrap("1"));
eq1(wrap("foo1bar").charAt(4), wrap("b"));
eq1(wrap("foo1bar").charAt(5), wrap("a"));
eq1(wrap("foo1bar").charAt(6), wrap("r"));
eq1(wrap("foo1bar").charAt(7), wrap(""));
eq1(wrap("foo1bar").charAt( -1), wrap(""));
eq1(wrap("").charAt(0), wrap(""));
eq1(wrap("").charAt(1), wrap(""));
eq1(wrap("").charAt( -1), wrap(""));



// charCodeAt
wrap("foo1bar").charCodeAt(0) == 102;
wrap("foo1bar").charCodeAt(1) == 111;
wrap("foo1bar").charCodeAt(2) == 111;
wrap("foo1bar").charCodeAt(3) == 49;
wrap("foo1bar").charCodeAt(4) == 98;
wrap("foo1bar").charCodeAt(5) == 97;
wrap("foo1bar").charCodeAt(6) == 114;
wrap("foo1bar").charCodeAt(7) == null;
wrap("foo1bar").charCodeAt( -1) == null;



// indexOf
var s = wrap("foo1bar");

s.indexOf(wrap("f")) == 0;

s.indexOf(wrap("o")) == 1;

s.indexOf(wrap("1")) == 3;
s.indexOf(wrap("b")) == 4;
s.indexOf(wrap("a")) == 5;
s.indexOf(wrap("r")) == 6;
s.indexOf(wrap("z")) == -1;
//s.indexOf(null) == -1;
//s.indexOf(null, 1) == -1;
//s.indexOf(null, -1) == -1;
s.indexOf(wrap("foo")) == 0;
s.indexOf(wrap("oo")) == 1;
//s.indexOf("bart") == -1;
//s.indexOf("r", -1) == -1;
//s.indexOf("r", -10) == -1;

s.indexOf(wrap("o"), 1) == 1;
s.indexOf(wrap("o"), 2) == 2;
s.indexOf(wrap("o"), 3) == -1;


var s = "foofoofoobarbar";
s.lastIndexOf("r") == 14;

// lastIndexOf
var s = wrap("foofoofoobarbar");

s.lastIndexOf(wrap("r")) == 14;

s.lastIndexOf(wrap("a")) == 13;
s.lastIndexOf(wrap("b")) == 12;

s.lastIndexOf(wrap("bar")) == 12;
s.lastIndexOf(wrap("foo")) == 6;
s.lastIndexOf(wrap("foofoo")) == 3;
s.lastIndexOf(wrap("f")) == 6;
s.lastIndexOf(wrap("barb")) == 9;
s.lastIndexOf(wrap("z")) == -1;
//s.lastIndexOf(null) == -1;
//s.lastIndexOf(null, 1) == -1;
//s.lastIndexOf(null, 14) == -1;

s.lastIndexOf(wrap("r"), 14) == 14;


s.lastIndexOf(wrap("r"), 13) == 11;

s.lastIndexOf(wrap("a"), 14) == 13;

s.lastIndexOf(wrap("a"), 13) == 13;
s.lastIndexOf(wrap("a"), 12) == 10;

s.lastIndexOf(wrap("bar"), 12) == 12;

s.lastIndexOf(wrap("bar"), 11) == 9;
s.lastIndexOf(wrap("bar"), 9) == 9;
s.lastIndexOf(wrap("bar"), 8) == -1;



// split
var s = wrap("xfooxfooxxbarxbarxx");
arrEq(s.split(wrap("x")),[wrap(""), wrap("foo"), wrap("foo"), wrap(""), wrap("bar"), wrap("bar"), wrap(""),wrap("")]);

arrEq(s.split(wrap("xx")),[wrap("xfooxfoo"),wrap("barxbar"),wrap("")]);

//s.split(wrap("")) == [wrap("x"), wrap("f"), wrap("o"), wrap("o"), wrap("x"), wrap("f"), wrap("o"), wrap("o"), wrap("x"), wrap("x"), wrap("b"), wrap("a"), wrap("r"), wrap("x"), wrap("b"), wrap("a"), wrap("r"), wrap("x"), wrap("x")];
//s.split(wrap("z")) == [wrap("xfooxfooxxbarxbarxx")];



var s = wrap("xfooxfooxxbarxbarxx");
eq1(s.substr(0),new haxe.i18n.Ucs2("xfooxfooxxbarxbarxx"));
eq1(s.substr(1), wrap("fooxfooxxbarxbarxx"));
eq1(s.substr(19),  wrap(""));
eq1(s.substr(18), wrap("x"));
eq1(s.substr(17), wrap("xx"));

eq1(s.substr(-1), wrap("x"));
eq1(s.substr(-2), wrap("xx"));
eq1(s.substr(-18), wrap("fooxfooxxbarxbarxx"));
eq1(s.substr(-19), wrap("xfooxfooxxbarxbarxx"));
eq1(s.substr( -100), wrap("xfooxfooxxbarxbarxx"));

eq1(s.substr(0, 0), wrap(""));
eq1(s.substr(0, 1), wrap("x"));
eq1(s.substr(0, 2), wrap("xf"));

eq1(s.substr(0, 100), wrap("xfooxfooxxbarxbarxx"));

eq1(s.substr(0, -1), wrap("xfooxfooxxbarxbarx"));
eq1(s.substr(0, -2), wrap("xfooxfooxxbarxbar"));

//eq1(s.substr(1, -2), wrap("fooxfooxxbarxbar"));
//eq1(s.substr(2, -2), wrap("ooxfooxxbarxbar"));
eq1(s.substr(0, -100), wrap(""));



// substring
var s = wrap("xfooxfooxxbarxbarxx");
eq1(s.substring(0, 0), wrap(""));
eq1(s.substring(0, 1), wrap("x"));
eq1(s.substring(1, 0), wrap("x"));
eq1(s.substring(0, 2), wrap("xf"));
eq1(s.substring(2, 0), wrap("xf"));
eq1(s.substring(-1, 0), wrap(""));
eq1(s.substring(0, -1), wrap(""));
eq1(s.substring(-1, -1), wrap(""));
eq1(s.substring(-1, 1), wrap("x"));
eq1(s.substring(1, -1), wrap("x"));
eq1(s.substring(-1, 2), wrap("xf"));
eq1(s.substring(2, -1), wrap("xf"));
eq1(s.substring(0), wrap("xfooxfooxxbarxbarxx"));
eq1(s.substring(1), wrap("fooxfooxxbarxbarxx"));
eq1(s.substring(2), wrap("ooxfooxxbarxbarxx"));
eq1(s.substring(0, -1), wrap(""));
eq1(s.substring(1, -1), wrap("x"));
eq1(s.substring(2, -1), wrap("xf"));
eq1(s.substring(20, 0), wrap("xfooxfooxxbarxbarxx"));
eq1(s.substring(0, 100), wrap("xfooxfooxxbarxbarxx"));
eq1(s.substring(100, 120), wrap(""));
eq1(s.substring(100, 0), wrap("xfooxfooxxbarxbarxx"));
eq1(s.substring(120, 100), wrap(""));

// fromCharCode
eq1(haxe.i18n.Ucs2.fromCharCode(65), wrap("A"));

// ensure int strings compared as strings, not parsed ints (issue #3734)
(wrap("3") > wrap("11")) == true;
(wrap(" 3") < wrap("3")) == true;


// ucs2 specific

var violine = 0x1D11E; // 𝄞.code 

wrap("𝄞").length == 2;


eq1(wrap("𝄞"), haxe.i18n.Ucs2.fromCharCode(violine));  

// ucs2 strings can store surrogate pairs, but they count as separate pairs

wrap("𝄞").charCodeAt(0) == 0xD834;
wrap("𝄞").charCodeAt(1) == 0xDD1E;

var x = haxe.i18n.Ucs2.fromCharCode(violine);
x.length == 2;
x.isValid() == false;

wrap("𝄞").isValid() == false;