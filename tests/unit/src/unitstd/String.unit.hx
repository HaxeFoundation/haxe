// new
var str = "foo";
var str2 = new String(str);
str == str2;

// toUpperCase
"foo".toUpperCase() == "FOO";
"_bar".toUpperCase() == "_BAR";
"123b".toUpperCase() == "123B";
"".toUpperCase() == "";
"A".toUpperCase() == "A";

// toLowerCase
"FOO".toLowerCase() == "foo";
"_BAR".toLowerCase() == "_bar";
"123B".toLowerCase() == "123b";
"".toLowerCase() == "";
"a".toLowerCase() == "a";

// charAt
var s = "foo1bar";
s.charAt(0) == "f";
s.charAt(1) == "o";
s.charAt(2) == "o";
s.charAt(3) == "1";
s.charAt(4) == "b";
s.charAt(5) == "a";
s.charAt(6) == "r";
s.charAt(7) == "";
s.charAt( -1) == "";
"".charAt(0) == "";
"".charAt(1) == "";
"".charAt( -1) == "";

// charCodeAt
var s = "foo1bar";
s.charCodeAt(0) == 102;
s.charCodeAt(1) == 111;
s.charCodeAt(2) == 111;
s.charCodeAt(3) == 49;
s.charCodeAt(4) == 98;
s.charCodeAt(5) == 97;
s.charCodeAt(6) == 114;
s.charCodeAt(7) == null;
s.charCodeAt( -1) == null;

// code
"f".code == 102;
"o".code == 111;
"1".code == 49;
"b".code == 98;
"a".code == 97;
"r".code == 114;

// indexOf
var s = "foo1bar";
s.indexOf("") == 0;
s.indexOf("f") == 0;
s.indexOf("o") == 1;
s.indexOf("1") == 3;
s.indexOf("b") == 4;
s.indexOf("a") == 5;
s.indexOf("r") == 6;
s.indexOf("z") == -1;
//s.indexOf(null) == -1;
//s.indexOf(null, 1) == -1;
//s.indexOf(null, -1) == -1;
s.indexOf("foo") == 0;
s.indexOf("oo") == 1;
//s.indexOf("bart") == -1;
//s.indexOf("r", -1) == -1;
//s.indexOf("r", -10) == -1;
s.indexOf("", 2) == 2;
s.indexOf("", 200) == s.length;
s.indexOf("o", 1) == 1;
s.indexOf("o", 2) == 2;
s.indexOf("o", 3) == -1;
//s.indexOf("", -10) == 0;
//s.indexOf("", 7) == 7; // see #8117
//s.indexOf("", 8) == -1; // see #8117
s.indexOf("r", 7) == -1;
s.indexOf("r", 8) == -1;

// lastIndexOf
var s = "foofoofoobarbar";
s.lastIndexOf("") == s.length;
s.lastIndexOf("r") == 14;
s.lastIndexOf("a") == 13;
s.lastIndexOf("b") == 12;
s.lastIndexOf("bar") == 12;
s.lastIndexOf("foo") == 6;
s.lastIndexOf("foofoo") == 3;
s.lastIndexOf("f") == 6;
s.lastIndexOf("barb") == 9;
s.lastIndexOf("barb", 12) == 9;
s.lastIndexOf("barb", 13) == 9;
s.lastIndexOf("z") == -1;
//s.lastIndexOf(null) == -1;
//s.lastIndexOf(null, 1) == -1;
//s.lastIndexOf(null, 14) == -1;
s.lastIndexOf("", 2) == 2;
s.lastIndexOf("", 200) == s.length;
s.lastIndexOf("r", 14) == 14;
s.lastIndexOf("r", 13) == 11;
s.lastIndexOf("a", 14) == 13;
s.lastIndexOf("a", 13) == 13;
s.lastIndexOf("a", 12) == 10;
s.lastIndexOf("bar", 12) == 12;
s.lastIndexOf("bar", 11) == 9;
s.lastIndexOf("bar", 9) == 9;
s.lastIndexOf("bar", 8) == -1;
s.lastIndexOf("a", s.length) == 13;
s.lastIndexOf("a", s.length + 9000) == 13;

// split
var s = "xfooxfooxxbarxbarxx";
s.split("x") == ["", "foo", "foo", "", "bar", "bar", "",""];
s.split("xx") == ["xfooxfoo","barxbar",""];
s.split("") == ["x", "f", "o", "o", "x", "f", "o", "o", "x", "x", "b", "a", "r", "x", "b", "a", "r", "x", "x"];
s.split("z") == ["xfooxfooxxbarxbarxx"];

// substr
var s = "xfooxfooxxbarxbarxx";
s.substr(0) == "xfooxfooxxbarxbarxx";
s.substr(1) == "fooxfooxxbarxbarxx";
s.substr(19) == "";
s.substr(18) == "x";
s.substr(17) == "xx";
s.substr(-1) == "x";
s.substr(-2) == "xx";
s.substr(-18) == "fooxfooxxbarxbarxx";
s.substr(-19) == "xfooxfooxxbarxbarxx";
s.substr( -100) == "xfooxfooxxbarxbarxx";
s.substr(0, 0) == "";
s.substr(0, 1) == "x";
s.substr(0, 2) == "xf";
s.substr(0, 100) == "xfooxfooxxbarxbarxx";
s.substr(0, -1) == "xfooxfooxxbarxbarx";
s.substr(0, -2) == "xfooxfooxxbarxbar";
//s.substr(1, -2) == "fooxfooxxbarxbar";
//s.substr(2, -2) == "ooxfooxxbarxbar";
s.substr(0, -100) == "";
var s = "01234567890";
s.substr(4, -3) == "4567";
s.substr(5, -5) == "5";
s.substr(5, -6) == "";
s.substr(5, -7) == "";
s.substr(-4, 4) == "4567";
s.substr(-4, -5) == "45";
s.substr(-4, -6) == "4";
s.substr(-4, -7) == "";
s.substr(-9) == "1234567890";
s.substr(-10) == "01234567890";
s.substr(-11) == "01234567890";
s.substr(-9, 3) == "123";
s.substr(-10, 3) == "012";
s.substr(-11, 3) == "012";
s.substr(0, -9) == "01";
s.substr(0, -8) == "0";
s.substr(0, -10) == "";
s.substr(0, -11) == "";

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
s.substring(20, 0) == "xfooxfooxxbarxbarxx";
s.substring(0, 100) == "xfooxfooxxbarxbarxx";
s.substring(100, 120) == "";
s.substring(100, 0) == "xfooxfooxxbarxbarxx";
s.substring(120, 100) == "";
s.substring(5, 8) == "foo";
s.substring(8, 5) == "foo";

// fromCharCode
String.fromCharCode(65) == "A";

// ensure int strings compared as strings, not parsed ints (issue #3734)
("3" > "11") == true;
(" 3" < "3") == true;

// string comparison (see #8332)
("a" < "b") == true;
("a" <= "b") == true;
("a" > "b") == false;
("a" >= "b") == false;

#if target.unicode
("𠜎zя" > "abя") == true;
("𠜎zя" >= "abя") == true;
("𠜎zя" < "abя") == false;
("𠜎zя" <= "abя") == false;

#if target.utf16
// since U+10002 in UTF16 is D800 DC02
("\u{FF61}" < "\u{10002}") == false;
#else
("\u{FF61}" < "\u{10002}") == true;
#end

#end
