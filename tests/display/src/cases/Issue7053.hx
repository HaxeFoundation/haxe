package cases;

class Issue7053 extends DisplayTestCase {
	/**
	{-1-}
	**/
	function testFull() {
		var fields = toplevel(pos(1));
		// technically, "package" should be here too. But I think the test system adds a
		// package statement automatically, so it is omitted.
		for (expected in ["import", "using", "private", "extern", "class", "interface", "enum", "abstract", "typedef", "final"]) {
			eq(true, hasField(fields, expected, null, "keyword"));
		}
	}

	/**
	import String;
	{-1-}
	**/
	function testAfterImport() {
		var fields = toplevel(pos(1));
		for (expected in ["import", "using", "private", "extern", "class", "interface", "enum", "abstract", "typedef", "final"]) {
			eq(true, hasField(fields, expected, null, "keyword"));
		}
	}

	/**
	class C { }
	{-1-}
	**/
	function testAfterClass() {
		var fields = toplevel(pos(1));
		for (expected in ["private", "extern", "class", "interface", "enum", "abstract", "typedef", "final"]) {
			eq(true, hasField(fields, expected, null, "keyword"));
		}
	}
}