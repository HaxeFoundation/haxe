package unit.teststd;

class TestUnicodeString extends unit.Test {
	public function test() {
		#if target.unicode
		var s = new UnicodeString("𠜎zя");
		var codes = [132878, 122, 1103];

		// length
		eq(s.length, codes.length);
		// // toUpperCase, toLowerCase
		// var turkishLower = "ğüşıiöç";
		// var turkishUpper = "ĞÜŞIİÖÇ";
		// turkishUpper == turkishLower.toUpperCase();
		// turkishLower == turkishUpper.toLowerCase();
		// charAt
		eq(s.charAt(0), "𠜎");
		eq(s.charAt(1), "z");
		eq(s.charAt(2), "я");
		eq(s.charAt(3), "");
		eq(s.charAt(-1), "");
		eq(("" : UnicodeString).charAt(0), "");
		eq(("" : UnicodeString).charAt(1), "");
		eq(("" : UnicodeString).charAt(-1), "");
		// charCodeAt
		eq(s.charCodeAt(0), codes[0]);
		eq(s.charCodeAt(1), codes[1]);
		eq(s.charCodeAt(2), codes[2]);
		eq(s.charCodeAt(3), null);
		eq(s.charCodeAt(-1), null);
		// indexOf
		var s:UnicodeString = "𠜎zяяw";
		eq(s.indexOf("𠜎"), 0);
		eq(s.indexOf("z"), 1);
		eq(s.indexOf("я"), 2);
		eq(s.indexOf("zя"), 1);
		eq(s.indexOf("w"), 4);
		eq(s.indexOf("яw"), 3);
		eq(s.indexOf("f"), -1);
		eq(s.indexOf("я", 0), 2);
		eq(s.indexOf("я", 1), 2);
		eq(s.indexOf("я", 2), 2);
		eq(s.indexOf("я", 3), 3);
		eq(s.indexOf("я", 4), -1);
		eq(s.indexOf("я", 40), -1);
		eq(s.indexOf("я", -1), 2);
		eq(s.indexOf("я", -2), 2);
		eq(s.indexOf("я", -3), 2);
		eq(s.indexOf("я", -4), 2);
		eq(s.indexOf("я", -5), 2);
		eq(s.indexOf("я", -50), 2);
		// empty string
		eq(s.indexOf(""), 0);
		eq(s.indexOf("", -1), 0);
		eq(s.indexOf("", 0), 0);
		eq(s.indexOf("", 1), 1);
		eq(s.indexOf("", 2), 2);
		eq(s.indexOf("", 3), 3);
		eq(s.indexOf("", 4), 4);
		eq(s.indexOf("", 5), 5);
		eq(s.indexOf("", 6), 5);
		// lastIndexOf
		var s:UnicodeString = "𠜎zяяw";
		eq(s.lastIndexOf("𠜎"), 0);
		eq(s.lastIndexOf("z"), 1);
		eq(s.lastIndexOf("я"), 3);
		eq(s.lastIndexOf("zя"), 1);
		eq(s.lastIndexOf("яw"), 3);
		eq(s.lastIndexOf("f"), -1);
		eq(s.lastIndexOf("я", 0), -1);
		eq(s.lastIndexOf("я", 1), -1);
		eq(s.lastIndexOf("я", 2), 2);
		eq(s.lastIndexOf("я", 3), 3);
		eq(s.lastIndexOf("я", 4), 3);
		eq(s.lastIndexOf("я", 40), 3);
		// substr
		var s:UnicodeString = "𠜎zяяw";
		eq(s.substr(0), "𠜎zяяw");
		eq(s.substr(1), "zяяw");
		eq(s.substr(5), "");
		eq(s.substr(4), "w");
		eq(s.substr(3), "яw");
		eq(s.substr(-1), "w");
		eq(s.substr(-2), "яw");
		eq(s.substr(-4), "zяяw");
		eq(s.substr(-5), "𠜎zяяw");
		eq(s.substr(-100), "𠜎zяяw");
		eq(s.substr(0, 0), "");
		eq(s.substr(0, 1), "𠜎");
		eq(s.substr(0, 2), "𠜎z");
		eq(s.substr(0, 100), "𠜎zяяw");
		eq(s.substr(0, -1), "𠜎zяя");
		eq(s.substr(0, -2), "𠜎zя");
		eq(s.substr(0, -100), "");
		// substring
		var s:UnicodeString = "𠜎zяяw";
		eq(s.substring(0, 0), "");
		eq(s.substring(0, 1), "𠜎");
		eq(s.substring(1, 0), "𠜎");
		eq(s.substring(0, 2), "𠜎z");
		eq(s.substring(2, 0), "𠜎z");
		eq(s.substring(-1, 0), "");
		eq(s.substring(0, -1), "");
		eq(s.substring(-1, -1), "");
		eq(s.substring(-1, 1), "𠜎");
		eq(s.substring(1, -1), "𠜎");
		eq(s.substring(-1, 2), "𠜎z");
		eq(s.substring(2, -1), "𠜎z");
		eq(s.substring(0), "𠜎zяяw");
		eq(s.substring(1), "zяяw");
		eq(s.substring(2), "яяw");
		eq(s.substring(0, -1), "");
		eq(s.substring(5, 0), "𠜎zяяw");
		eq(s.substring(0, 100), "𠜎zяяw");
		eq(s.substring(100, 120), "");
		eq(s.substring(100, 0), "𠜎zяяw");
		eq(s.substring(120, 100), "");
		eq(s.substring(1, 4), "zяя");
		eq(s.substring(4, 1), "zяя");
		var s = new UnicodeString("𠜎zя");

		// @:op(UnicodeString)
		var s2 = new UnicodeString("𠜎z");
		t(s != s2);
		!(s == s2);
		t(s > s2);
		t(s >= s2);
		t(s2 < s);
		t(s2 <= s);
		eq((s + s2).length, s.length + s2.length);
		var s3 = s;
		eq((s3 += s2).length, s.length + s2.length);
		// @:op(String)
		var s2 = "abя";
		t(s != s2);
		!(s == s2);
		t(s > s2);
		t(s >= s2);
		t(s2 < s);
		t(s2 <= s);
		eq((s + s2).length, s.length + (s2 : UnicodeString).length);
		var s3 = s;
		eq((s3 += s2).length, s.length + (s2 : UnicodeString).length);
		// iterator
		aeq(codes, [for (c in s) c]);
		// keyValueIterator
		var keys = [for (i in 0...codes.length) i];
		var actualKeyCodes = [for (i => c in s) [i, c]];
		aeq(keys, actualKeyCodes.map(a -> a[0]));
		aeq(codes, actualKeyCodes.map(a -> a[1]));
		// validate
		t(UnicodeString.validate(haxe.io.Bytes.ofHex("f0a9b8bde38182c3ab61"), UTF8));
		t(UnicodeString.validate(haxe.io.Bytes.ofHex("ed9fbf"), UTF8));
		t(UnicodeString.validate(haxe.io.Bytes.ofHex("ee8080"), UTF8));
		t(UnicodeString.validate(haxe.io.Bytes.ofHex("f48fbfbf"), UTF8));
		f(UnicodeString.validate(haxe.io.Bytes.ofHex("f0a9b8bde381c3ab61"), UTF8));
		UnicodeString.validate(haxe.io.Bytes.ofHex("c0af"), UTF8) == false; // overlong sequence
		UnicodeString.validate(haxe.io.Bytes.ofHex("eda080"), UTF8) == false; // surrogate byte sequence
		UnicodeString.validate(haxe.io.Bytes.ofHex("edbfbf"), UTF8) == false; // surrogate byte sequence
		UnicodeString.validate(haxe.io.Bytes.ofHex("f4908080"), UTF8) == false; // U+110000
		#else
		eq(1, 1);
		#end

	}
}
