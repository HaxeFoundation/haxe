package unit.teststd;

class TestString extends unit.Test {
	public function test() {
		// new
		var str = "foo";
		var str2 = new String(str);
		eq(str, str2);
		// toUpperCase
		eq("foo".toUpperCase(), "FOO");
		eq("_bar".toUpperCase(), "_BAR");
		eq("123b".toUpperCase(), "123B");
		eq("".toUpperCase(), "");
		eq("A".toUpperCase(), "A");
		// toLowerCase
		eq("FOO".toLowerCase(), "foo");
		eq("_BAR".toLowerCase(), "_bar");
		eq("123B".toLowerCase(), "123b");
		eq("".toLowerCase(), "");
		eq("a".toLowerCase(), "a");
		// charAt
		var s = "foo1bar";
		eq(s.charAt(0), "f");
		eq(s.charAt(1), "o");
		eq(s.charAt(2), "o");
		eq(s.charAt(3), "1");
		eq(s.charAt(4), "b");
		eq(s.charAt(5), "a");
		eq(s.charAt(6), "r");
		eq(s.charAt(7), "");
		eq(s.charAt(-1), "");
		eq("".charAt(0), "");
		eq("".charAt(1), "");
		eq("".charAt(-1), "");
		// charCodeAt
		var s = "foo1bar";
		eq(s.charCodeAt(0), 102);
		eq(s.charCodeAt(1), 111);
		eq(s.charCodeAt(2), 111);
		eq(s.charCodeAt(3), 49);
		eq(s.charCodeAt(4), 98);
		eq(s.charCodeAt(5), 97);
		eq(s.charCodeAt(6), 114);
		eq(s.charCodeAt(7), null);
		eq(s.charCodeAt(-1), null);
		// code
		eq("f".code, 102);
		eq("o".code, 111);
		eq("1".code, 49);
		eq("b".code, 98);
		eq("a".code, 97);
		eq("r".code, 114);
		// indexOf
		var s = "foo1bar";
		eq(s.indexOf(""), 0);
		eq(s.indexOf("f"), 0);
		eq(s.indexOf("o"), 1);
		eq(s.indexOf("1"), 3);
		eq(s.indexOf("b"), 4);
		eq(s.indexOf("a"), 5);
		eq(s.indexOf("r"), 6);
		eq(s.indexOf("z"), -1);
		// empty string
		eq(s.indexOf(""), 0);
		eq(s.indexOf("", -1), 0);
		eq(s.indexOf("", 0), 0);
		eq(s.indexOf("", 1), 1);
		eq(s.indexOf("", 2), 2);
		eq(s.indexOf("", 3), 3);
		eq(s.indexOf("", 4), 4);
		eq(s.indexOf("", 5), 5);
		eq(s.indexOf("", 6), 6);
		eq(s.indexOf("", 7), 7);
		eq(s.indexOf("", 8), 7);
		// negative startIndex
		eq(s.indexOf("f", -1), 0);
		eq(s.indexOf("o", -1), 1);
		eq(s.indexOf("1", -1), 3);
		eq(s.indexOf("b", -1), 4);
		eq(s.indexOf("a", -1), 5);
		eq(s.indexOf("r", -1), 6);
		eq(s.indexOf("z", -1), -1);
		// startIndex >= length
		eq(s.indexOf("f", 7), -1);
		eq(s.indexOf("o", 7), -1);
		eq(s.indexOf("1", 7), -1);
		eq(s.indexOf("b", 7), -1);
		eq(s.indexOf("a", 7), -1);
		eq(s.indexOf("r", 7), -1);
		eq(s.indexOf("z", 7), -1);
		// s.indexOf(null) == -1;
		// s.indexOf(null, 1) == -1;
		// s.indexOf(null, -1) == -1;
		eq(s.indexOf("foo"), 0);
		eq(s.indexOf("oo"), 1);
		// s.indexOf("bart") == -1;
		eq(s.indexOf("", 2), 2);
		eq(s.indexOf("", 200), s.length);
		eq(s.indexOf("o", 1), 1);
		eq(s.indexOf("o", 2), 2);
		eq(s.indexOf("o", 3), -1);
		eq(s.indexOf("r", 7), -1);
		eq(s.indexOf("r", 8), -1);
		// lastIndexOf
		var s = "foofoofoobarbar";
		eq(s.lastIndexOf(""), s.length);
		eq(s.lastIndexOf("r"), 14);
		eq(s.lastIndexOf("a"), 13);
		eq(s.lastIndexOf("b"), 12);
		eq(s.lastIndexOf("bar"), 12);
		eq(s.lastIndexOf("foo"), 6);
		eq(s.lastIndexOf("foofoo"), 3);
		eq(s.lastIndexOf("f"), 6);
		eq(s.lastIndexOf("barb"), 9);
		eq(s.lastIndexOf("barb", 12), 9);
		eq(s.lastIndexOf("barb", 13), 9);
		eq(s.lastIndexOf("z"), -1);
		// s.lastIndexOf(null) == -1;
		// s.lastIndexOf(null, 1) == -1;
		// s.lastIndexOf(null, 14) == -1;
		eq(s.lastIndexOf("", 2), 2);
		eq(s.lastIndexOf("", 200), s.length);
		eq(s.lastIndexOf("r", 14), 14);
		eq(s.lastIndexOf("r", 13), 11);
		eq(s.lastIndexOf("a", 14), 13);
		eq(s.lastIndexOf("a", 13), 13);
		eq(s.lastIndexOf("a", 12), 10);
		eq(s.lastIndexOf("bar", 12), 12);
		eq(s.lastIndexOf("bar", 11), 9);
		eq(s.lastIndexOf("bar", 9), 9);
		eq(s.lastIndexOf("bar", 8), -1);
		eq(s.lastIndexOf("a", s.length), 13);
		eq(s.lastIndexOf("a", s.length + 9000), 13);
		// split
		var s = "xfooxfooxxbarxbarxx";
		aeq(["", "foo", "foo", "", "bar", "bar", "", ""], s.split("x"));
		aeq(["xfooxfoo", "barxbar", ""], s.split("xx"));
		aeq(["x", "f", "o", "o", "x", "f", "o", "o", "x", "x", "b", "a", "r", "x", "b", "a", "r", "x", "x"], s.split(""));
		eq(s.split("z")[0], "xfooxfooxxbarxbarxx");
		// substr
		var s = "xfooxfooxxbarxbarxx";
		eq(s.substr(0), "xfooxfooxxbarxbarxx");
		eq(s.substr(1), "fooxfooxxbarxbarxx");
		eq(s.substr(19), "");
		eq(s.substr(18), "x");
		eq(s.substr(17), "xx");
		eq(s.substr(-1), "x");
		eq(s.substr(-2), "xx");
		eq(s.substr(-18), "fooxfooxxbarxbarxx");
		eq(s.substr(-19), "xfooxfooxxbarxbarxx");
		eq(s.substr(-100), "xfooxfooxxbarxbarxx");
		eq(s.substr(0, 0), "");
		eq(s.substr(0, 1), "x");
		eq(s.substr(0, 2), "xf");
		eq(s.substr(0, 100), "xfooxfooxxbarxbarxx");
		eq(s.substr(0, -1), "xfooxfooxxbarxbarx");
		eq(s.substr(0, -2), "xfooxfooxxbarxbar");
		eq(s.substr(1, -2), "fooxfooxxbarxbar");
		eq(s.substr(2, -2), "ooxfooxxbarxbar");
		eq(s.substr(0, -100), "");
		eq(s.substr(-19, -2), "xfooxfooxxbarxbar");
		eq(s.substr(-18, -2), "fooxfooxxbarxbar");
		eq(s.substr(-20, -2), "xfooxfooxxbarxbar");
		eq(s.substr(-100, -2), "xfooxfooxxbarxbar");
		eq(s.substr(-19, 2), "xf");
		eq(s.substr(-18, 2), "fo");
		eq(s.substr(-20, 2), "xf");
		eq(s.substr(-100, 2), "xf");
		eq(s.substr(-100, -100), "");
		eq(s.substr(17, -1), "x");
		eq(s.substr(17, -2), "");
		eq(s.substr(17, -10), "");
		eq(s.substr(17, -100), "");
		// substring
		var s = "xfooxfooxxbarxbarxx";
		eq(s.substring(0, 0), "");
		eq(s.substring(0, 1), "x");
		eq(s.substring(1, 0), "x");
		eq(s.substring(0, 2), "xf");
		eq(s.substring(2, 0), "xf");
		eq(s.substring(-1, 0), "");
		eq(s.substring(0, -1), "");
		eq(s.substring(-1, -1), "");
		eq(s.substring(-1, 1), "x");
		eq(s.substring(1, -1), "x");
		eq(s.substring(-1, 2), "xf");
		eq(s.substring(2, -1), "xf");
		eq(s.substring(0), "xfooxfooxxbarxbarxx");
		eq(s.substring(1), "fooxfooxxbarxbarxx");
		eq(s.substring(2), "ooxfooxxbarxbarxx");
		eq(s.substring(20, 0), "xfooxfooxxbarxbarxx");
		eq(s.substring(0, 100), "xfooxfooxxbarxbarxx");
		eq(s.substring(100, 120), "");
		eq(s.substring(100, 0), "xfooxfooxxbarxbarxx");
		eq(s.substring(120, 100), "");
		eq(s.substring(5, 8), "foo");
		eq(s.substring(8, 5), "foo");
		// fromCharCode
		eq(String.fromCharCode(65), "A");
		// ensure int strings compared as strings, not parsed ints (issue #3734)
		t(("3" > "11"));
		t((" 3" < "3"));
		// string comparison (see #8332)
		t(("a" < "b"));
		t(("a" <= "b"));
		f(("a" > "b"));
		f(("a" >= "b"));
		#if target.unicode
		t(("𠜎zя" > "abя"));
		t(("𠜎zя" >= "abя"));
		f(("𠜎zя" < "abя"));
		f(("𠜎zя" <= "abя"));
		#if target.utf16
		// since U+10002 in UTF16 is D800 DC02
		f(("\u{FF61}" < "\u{10002}"));
		#else
		t(("\u{FF61}" < "\u{10002}"));
		#end
		#end

	}
}
