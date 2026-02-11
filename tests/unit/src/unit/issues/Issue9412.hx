package unit.issues;

class Issue9412 extends unit.Test {
#if lua
	function testLength() {
		// ASCII
		eq("hello".length, 5);
		// Multi-byte: "héllo" has 5 characters but more bytes
		eq("h\u00E9llo".length, 5);
		// CJK: 3 characters
		eq("\u4F60\u597D\u554A".length, 3);
		eq("".length, 0);
	}

	function testCharAt() {
		var s = "h\u00E9llo";
		eq(s.charAt(0), "h");
		eq(s.charAt(1), "\u00E9");
		eq(s.charAt(2), "l");
		eq(s.charAt(4), "o");
		eq(s.charAt(10), "");
	}

	function testCharCodeAt() {
		var s = "h\u00E9llo";
		eq(s.charCodeAt(0), 0x68); // 'h'
		eq(s.charCodeAt(1), 0xE9); // 'é'
		eq(s.charCodeAt(2), 0x6C); // 'l'
	}

	function testIndexOf() {
		var s = "h\u00E9llo";
		eq(s.indexOf("h"), 0);
		eq(s.indexOf("\u00E9"), 1);
		eq(s.indexOf("llo"), 2);
		eq(s.indexOf("z"), -1);
	}

	function testSubstring() {
		var s = "h\u00E9llo";
		eq(s.substring(0, 1), "h");
		eq(s.substring(1, 2), "\u00E9");
		eq(s.substring(0, 5), "h\u00E9llo");
		eq(s.substring(2), "llo");
	}

	function testToUpperLowerCase() {
		// ASCII cases always work
		eq("hello".toUpperCase(), "HELLO");
		eq("HELLO".toLowerCase(), "hello");
	}

	function testFromCharCode() {
		eq(String.fromCharCode(0x68), "h");
		eq(String.fromCharCode(0xE9), "\u00E9");
	}

	function testSplit() {
		var parts = "a\u00E9b\u00E9c".split("\u00E9");
		eq(parts.length, 3);
		eq(parts[0], "a");
		eq(parts[1], "b");
		eq(parts[2], "c");
	}
#else
	function test() {
		noAssert();
	}
#end
}
