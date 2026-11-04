package unit.teststd;

class TestStringTools extends unit.Test {
	public function test() {
		// htmlEscape
		var str = "<foo> & <bar> = 'invalid\"'";
		var strEsc = "&lt;foo&gt; &amp; &lt;bar&gt; = 'invalid\"'";
		var strEscQuotes = "&lt;foo&gt; &amp; &lt;bar&gt; = &#039;invalid&quot;&#039;";
		eq(StringTools.htmlEscape(str, false), strEsc);
		eq(StringTools.htmlEscape(str, true), strEscQuotes);

		// htmlUnescape
		eq(StringTools.htmlUnescape(strEsc), str);
		eq(StringTools.htmlUnescape(strEscQuotes), str);

		// startsWith
		t(StringTools.startsWith("foo", "f"));
		t(StringTools.startsWith("foo", "fo"));
		t(StringTools.startsWith("foo", "foo"));
		f(StringTools.startsWith("foo", "fooo"));
		t(StringTools.startsWith("foo", ""));
		t(StringTools.startsWith("", ""));

		// endsWith
		t(StringTools.endsWith("foo", "o"));
		t(StringTools.endsWith("foo", "oo"));
		t(StringTools.endsWith("foo", "foo"));
		f(StringTools.endsWith("foo", "fooo"));
		t(StringTools.endsWith("foo", ""));
		t(StringTools.endsWith("", ""));
		t(StringTools.endsWith("μου\n","\n"));

		// isSpace
		f(StringTools.isSpace("", 0));
		f(StringTools.isSpace("", 1));
		f(StringTools.isSpace(" ", -1));
		f(StringTools.isSpace("a", 0));
		t(StringTools.isSpace("  ", 0));
		t(StringTools.isSpace(" ", 0));
		t(StringTools.isSpace(" a", 0));
		t(StringTools.isSpace(String.fromCharCode(9), 0));
		t(StringTools.isSpace(String.fromCharCode(10), 0));
		t(StringTools.isSpace(String.fromCharCode(11), 0));
		t(StringTools.isSpace(String.fromCharCode(12), 0));
		t(StringTools.isSpace(String.fromCharCode(13), 0));

		// ltrim
		eq(StringTools.ltrim("a"), "a");
		eq(StringTools.ltrim("  a"), "a");
		eq(StringTools.ltrim("  a b"), "a b");
		eq(StringTools.ltrim("    "), "");
		eq(StringTools.ltrim(""), "");

		// rtrim
		eq(StringTools.rtrim("a"), "a");
		eq(StringTools.rtrim("a  "), "a");
		eq(StringTools.rtrim("a b  "), "a b");
		eq(StringTools.rtrim("    "), "");
		eq(StringTools.rtrim(""), "");

		// trim
		eq(StringTools.trim("a"), "a");
		eq(StringTools.trim("a  "), "a");
		eq(StringTools.trim("a b  "), "a b");
		eq(StringTools.trim("    "), "");
		eq(StringTools.trim(""), "");
		eq(StringTools.trim("  a"), "a");
		eq(StringTools.trim("  a b"), "a b");
		eq(StringTools.trim("  a b  "), "a b");

		// lpad
		eq(StringTools.lpad("", "", 2), "");
		eq(StringTools.lpad("", "a", 0), "");
		eq(StringTools.lpad("b", "a", 0), "b");
		eq(StringTools.lpad("b", "", 2), "b");
		eq(StringTools.lpad("", "a", 2), "aa");
		eq(StringTools.lpad("b", "a", 0), "b");
		eq(StringTools.lpad("b", "a", 1), "b");
		eq(StringTools.lpad("b", "a", 2), "ab");
		eq(StringTools.lpad("b", "a", 3), "aab");
		eq(StringTools.lpad("b", "a", 4), "aaab");
		eq(StringTools.lpad("b", "abcdef", 4), "abcdefb");

		// rpad
		eq(StringTools.rpad("", "", 2), "");
		eq(StringTools.rpad("", "a", 0), "");
		eq(StringTools.rpad("b", "a", 0), "b");
		eq(StringTools.rpad("b", "", 2), "b");
		eq(StringTools.rpad("", "a", 2), "aa");
		eq(StringTools.rpad("b", "a", 0), "b");
		eq(StringTools.rpad("b", "a", 1), "b");
		eq(StringTools.rpad("b", "a", 2), "ba");
		eq(StringTools.rpad("b", "a", 3), "baa");
		eq(StringTools.rpad("b", "a", 4), "baaa");
		eq(StringTools.rpad("b", "abcdef", 4), "babcdef");

		// replace
		var s = "xfooxfooxxbarxbarxx";
		eq(StringTools.replace(s, "x", ""), "foofoobarbar");
		eq(StringTools.replace(s, "", ""), "xfooxfooxxbarxbarxx");
		eq(StringTools.replace(s, "", "x"), "xxfxoxoxxxfxoxoxxxxxbxaxrxxxbxaxrxxxx");

		// hex
		eq(StringTools.hex(0, 0), "0");
		eq(StringTools.hex(0, 1), "0");
		eq(StringTools.hex(0, 2), "00");
		eq(StringTools.hex(1, 2), "01");
		eq(StringTools.hex(4564562), "45A652");
		eq(StringTools.hex(4564562, 0), "45A652");
		eq(StringTools.hex(4564562, 1), "45A652");
		eq(StringTools.hex( -1), "FFFFFFFF");
		eq(StringTools.hex( -2), "FFFFFFFE");
		eq(StringTools.hex(0xABCDEF, 7), "0ABCDEF");
		eq(StringTools.hex( -1, 8), "FFFFFFFF");
		eq(StringTools.hex( -481400000, 8), "E34E6B40");

		// contains
		var s = "foo1bar";
		t(StringTools.contains(s, ''));
		t(StringTools.contains(s, 'bar'));
		f(StringTools.contains(s, 'test'));

		// fastCodeAt
		var s = "foo1bar";
		eq(StringTools.fastCodeAt(s, 0), 102);
		eq(StringTools.fastCodeAt(s, 1), 111);
		eq(StringTools.fastCodeAt(s, 2), 111);
		eq(StringTools.fastCodeAt(s, 3), 49);
		eq(StringTools.fastCodeAt(s, 4), 98);
		eq(StringTools.fastCodeAt(s, 5), 97);
		eq(StringTools.fastCodeAt(s, 6), 114);
		var str = "abc";
		eq(StringTools.fastCodeAt(str, 0), "a".code);
		eq(StringTools.fastCodeAt(str, 1), "b".code);
		eq(StringTools.fastCodeAt(str, 2), "c".code);
		eq(StringTools.fastCodeAt(String.fromCharCode(128), 0), 128);
		eq(StringTools.fastCodeAt(String.fromCharCode(255), 0), 255);
		f(StringTools.isEof(StringTools.fastCodeAt(str, 0)));
		f(StringTools.isEof(StringTools.fastCodeAt(str, 1)));
		f(StringTools.isEof(StringTools.fastCodeAt(str, 2)));
		t(StringTools.isEof(StringTools.fastCodeAt(str, 3)));
		f(StringTools.isEof(StringTools.fastCodeAt(str, 2)));
		t(StringTools.isEof(StringTools.fastCodeAt(str, 3)));
		t(StringTools.isEof(StringTools.fastCodeAt("", 0)));

		// isEOF
		#if (neko || lua || eval)
		t(StringTools.isEof(null));
		#elseif (java || python)
		t(StringTools.isEof( -1));
		#elseif js
		// how do I test this here?
		#else
		t(StringTools.isEof(0));
		#end

		// iterators via @:using
		var s = 'zя𠜎';
		#if !(target.unicode)
		var expectedCodes = [122, 209, 143, 240, 160, 156, 142];
		#elseif utf16
		var expectedCodes = [122, 1103, 55361, 57102];
		#else
		var expectedCodes = [122, 1103, 132878];
		#end
		var expectedKeys = [for(i in 0...expectedCodes.length) i];
		// iterator()
		aeq(expectedCodes, [for(c in StringTools.iterator(s)) c]);
		// keyValueIterator()
		var keyCodes = [for(i => c in StringTools.keyValueIterator(s)) [i, c]];
		aeq(expectedKeys, keyCodes.map(a -> a[0]));
		aeq(expectedCodes, keyCodes.map(a -> a[1]));

	}
}
