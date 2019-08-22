package cases;

class Issue7053 extends DisplayTestCase {
	/**
		{-1-}
	**/
	function testFull() {
		var fields = toplevel(pos(1));
		// technically, "package" should be here too. But I think the test system adds a
		// package statement automatically, so it is omitted.
		for (expected in [
			"import", "using", "private", "extern", "class", "interface", "enum", "abstract", "typedef", "final"
		]) {
			eq(true, hasField(fields, expected, null, "keyword"));
		}
	}

	/**
		i{-1-}
	**/
	function testFullIdent() {
		var fields = toplevel(pos(1));
		// technically, "package" should be here too. But I think the test system adds a
		// package statement automatically, so it is omitted.
		for (expected in [
			"import", "using", "private", "extern", "class", "interface", "enum", "abstract", "typedef", "final"
		]) {
			eq(true, hasField(fields, expected, null, "keyword"));
		}
	}

	/**
		import String;
		{-1-}
	**/
	function testAfterImport() {
		var fields = toplevel(pos(1));
		for (expected in [
			"import", "using", "private", "extern", "class", "interface", "enum", "abstract", "typedef", "final"
		]) {
			eq(true, hasField(fields, expected, null, "keyword"));
		}
		for (unexpected in ["package"]) {
			eq(false, hasField(fields, unexpected, null, "keyword"));
		}
	}

	/**
		import String;
		i{-1-}
	**/
	function testAfterImportIdent() {
		var fields = toplevel(pos(1));
		for (expected in [
			"import", "using", "private", "extern", "class", "interface", "enum", "abstract", "typedef", "final"
		]) {
			eq(true, hasField(fields, expected, null, "keyword"));
		}
		for (unexpected in ["package"]) {
			eq(false, hasField(fields, unexpected, null, "keyword"));
		}
	}

	/**
		class C { }
		{-1-}
	**/
	function testAfterClass() {
		var fields = toplevel(pos(1));
		for (expected in [
			"private",
			"extern",
			"class",
			"interface",
			"enum",
			"abstract",
			"typedef",
			"final"
		]) {
			eq(true, hasField(fields, expected, null, "keyword"));
		}
		for (unexpected in ["import", "using", "package"]) {
			eq(false, hasField(fields, unexpected, null, "keyword"));
		}
	}

	/**
		class C { }
		i{-1-}
	**/
	function testAfterClassIdent() {
		var fields = toplevel(pos(1));
		for (expected in [
			"private",
			"extern",
			"class",
			"interface",
			"enum",
			"abstract",
			"typedef",
			"final"
		]) {
			eq(true, hasField(fields, expected, null, "keyword"));
		}
		for (unexpected in ["import", "using", "package"]) {
			eq(false, hasField(fields, unexpected, null, "keyword"));
		}
	}

	/**
		{-1-}class
	**/
	function testOnClass() {
		var fields = toplevel(pos(1));
		// technically, "package" should be here too. But I think the test system adds a
		// package statement automatically, so it is omitted.
		for (expected in [
			"import", "using", "private", "extern", "class", "interface", "enum", "abstract", "typedef", "final"
		]) {
			eq(true, hasField(fields, expected, null, "keyword"));
		}
	}

	/**
		private {-1-}
	**/
	function testAfterPrivate() {
		var fields = toplevel(pos(1));
		for (expected in ["extern", "class", "interface", "enum", "abstract", "typedef", "final"]) {
			eq(true, hasField(fields, expected, null, "keyword"));
		}
		for (unexpected in ["private"]) {
			eq(false, hasField(fields, unexpected, null, "keyword"));
		}
	}

	/**
		extern {-1-}
	**/
	function testAFterExtern() {
		var fields = toplevel(pos(1));
		for (expected in ["private", "class", "interface", "enum", "abstract", "typedef", "final"]) {
			eq(true, hasField(fields, expected, null, "keyword"));
		}
		for (unexpected in ["extern"]) {
			eq(false, hasField(fields, unexpected, null, "keyword"));
		}
	}

	/**
		final {-1-}
	**/
	function testAFterFinal() {
		var fields = toplevel(pos(1));
		for (expected in ["private", "class", "interface", "extern"]) {
			eq(true, hasField(fields, expected, null, "keyword"));
		}
		for (unexpected in ["final", "enum", "typedef", "abstract"]) {
			eq(false, hasField(fields, unexpected, null, "keyword"));
		}
	}

	/**
		final extern {-1-}
	**/
	function testAFterFinalExtern() {
		var fields = toplevel(pos(1));
		for (expected in ["private", "class", "interface"]) {
			eq(true, hasField(fields, expected, null, "keyword"));
		}
		for (unexpected in ["final", "enum", "typedef", "abstract", "extern"]) {
			eq(false, hasField(fields, unexpected, null, "keyword"));
		}
	}

	/**
		{-1-}
	**/
	@:filename("import.hx")
	function testInImportHx() {
		var keywords = toplevel(pos(1));
		var expectedKeywords = ["import", "using"];
		for (expected in expectedKeywords) {
			eq(true, hasField(keywords, expected, null, "keyword"));
		}
		eq(expectedKeywords.length, keywords.length);
	}
}
