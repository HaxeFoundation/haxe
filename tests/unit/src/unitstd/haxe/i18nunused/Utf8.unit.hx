// new

var wrap = function (s) return new haxe.i18n.Utf8(s);

var str = wrap("foo");
var str2 = str;
str == str2;



// toUpperCase
wrap("foo").toUpperCase() == wrap("FOO");
wrap("_bar").toUpperCase() == wrap("_BAR");
wrap("123b").toUpperCase() == wrap("123B");
wrap("").toUpperCase() == wrap("");
wrap("A").toUpperCase() == wrap("A");

// toLowerCase
wrap("FOO").toLowerCase() == wrap("foo");
wrap("_BAR").toLowerCase() == wrap("_bar");
wrap("123B").toLowerCase() == wrap("123b");
wrap("").toLowerCase() == wrap("");
wrap("a").toLowerCase() == wrap("a");






// charAt
var s = wrap("foo1bar");
s.charAt(0) == wrap("f");
s.charAt(1) == wrap("o");
s.charAt(2) == wrap("o");
s.charAt(3) == wrap("1");
s.charAt(4) == wrap("b");
s.charAt(5) == wrap("a");
s.charAt(6) == wrap("r");
s.charAt(7) == wrap("");
s.charAt( -1) == wrap("");
wrap("").charAt(0) == wrap("");
wrap("").charAt(1) == wrap("");
wrap("").charAt( -1) == wrap("");



// charCodeAt
var s = wrap("foo1bar");
s.charCodeAt(0) == 102;
s.charCodeAt(1) == 111;
s.charCodeAt(2) == 111;
s.charCodeAt(3) == 49;
s.charCodeAt(4) == 98;
s.charCodeAt(5) == 97;
s.charCodeAt(6) == 114;
s.charCodeAt(7) == null;
s.charCodeAt( -1) == null;



// reverse

var s = wrap("foo1bar");
wrap("rab1oof") == s.reverse();
wrap("") == wrap("").reverse();
wrap("a") == wrap("a").reverse();
wrap("ab") == wrap("ba").reverse();


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
s.split(wrap("x")) == [wrap(""), wrap("foo"), wrap("foo"), wrap(""), wrap("bar"), wrap("bar"), wrap(""),wrap("")];
s.split(wrap("xx")) == [wrap("xfooxfoo"),wrap("barxbar"),wrap("")];
//s.split(wrap("")) == [wrap("x"), wrap("f"), wrap("o"), wrap("o"), wrap("x"), wrap("f"), wrap("o"), wrap("o"), wrap("x"), wrap("x"), wrap("b"), wrap("a"), wrap("r"), wrap("x"), wrap("b"), wrap("a"), wrap("r"), wrap("x"), wrap("x")];
//s.split(wrap("z")) == [wrap("xfooxfooxxbarxbarxx")];

// substr
var s = wrap("xfooxfooxxbarxbarxx");
s.substr(0) == wrap("xfooxfooxxbarxbarxx");
s.substr(1) == wrap("fooxfooxxbarxbarxx");
s.substr(19) == wrap("");
s.substr(18) == wrap("x");
s.substr(17) == wrap("xx");
s.substr(-1) == wrap("x");
s.substr(-2) == wrap("xx");
s.substr(-18) == wrap("fooxfooxxbarxbarxx");
s.substr(-19) == wrap("xfooxfooxxbarxbarxx");
s.substr( -100) == wrap("xfooxfooxxbarxbarxx");
s.substr(0, 0) == wrap("");
s.substr(0, 1) == wrap("x");
s.substr(0, 2) == wrap("xf");
s.substr(0, 100) == wrap("xfooxfooxxbarxbarxx");
s.substr(0, -1) == wrap("xfooxfooxxbarxbarx");
s.substr(0, -2) == wrap("xfooxfooxxbarxbar");
//s.substr(1, -2) == wrap("fooxfooxxbarxbar");
//s.substr(2, -2) == wrap("ooxfooxxbarxbar");
s.substr(0, -100) == wrap("");


// substring
var s = "xfooxfooxxbarxbarxx";
s.substring(0, 0) == "";
s.substring(0, 1) == "x";
s.substring(1, 0) == "x";
s.substring(0, 2) == "xf";
s.substring(2, 0) == "xf";
s.substring(-1, 0) == "";
s.substring(0, -1) == "";
s.substring(-1, -1) == "";
s.substring(-1, 1) == "x";
s.substring(1, -1) == "x";
s.substring(-1, 2) == "xf";
s.substring(2, -1) == "xf";
s.substring(0) == "xfooxfooxxbarxbarxx";
s.substring(1) == "fooxfooxxbarxbarxx";
s.substring(2) == "ooxfooxxbarxbarxx";
s.substring(0, -1) == "";
s.substring(1, -1) == "x";
s.substring(2, -1) == "xf";
s.substring(20, 0) == "xfooxfooxxbarxbarxx";
s.substring(0, 100) == "xfooxfooxxbarxbarxx";
s.substring(100, 120) == "";
s.substring(100, 0) == "xfooxfooxxbarxbarxx";
s.substring(120, 100) == "";

// fromCharCode
String.fromCharCode(65) == "A";

// ensure int strings compared as strings, not parsed ints (issue #3734)
("3" > "11") == true;
(" 3" < "3") == true;

