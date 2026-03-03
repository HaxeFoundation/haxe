package cases.display.issues;

class Issue7053 extends DisplayTestCase {
	/**
		{-1-}
	**/
	function testFull(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		for (expected in ["import", "using", "private", "extern", "class", "interface", "enum", "abstract", "typedef", "final"]) {
			assertHasCompletion(result, item -> item.kind == Keyword && item.args.name == expected);
		}
	}

	/**
		i{-1-}
	**/
	function testFullIdent(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		for (expected in ["import", "using", "private", "extern", "class", "interface", "enum", "abstract", "typedef", "final"]) {
			assertHasCompletion(result, item -> item.kind == Keyword && item.args.name == expected);
		}
	}

	/**
		import String;
		{-1-}
	**/
	function testAfterImport(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		for (expected in ["import", "using", "private", "extern", "class", "interface", "enum", "abstract", "typedef", "final"]) {
			assertHasCompletion(result, item -> item.kind == Keyword && item.args.name == expected);
		}
		assertHasNoCompletion(result, item -> item.kind == Keyword && item.args.name == "package");
	}

	/**
		import String;
		i{-1-}
	**/
	function testAfterImportIdent(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		for (expected in ["import", "using", "private", "extern", "class", "interface", "enum", "abstract", "typedef", "final"]) {
			assertHasCompletion(result, item -> item.kind == Keyword && item.args.name == expected);
		}
		assertHasNoCompletion(result, item -> item.kind == Keyword && item.args.name == "package");
	}

	/**
		class C { }
		{-1-}
	**/
	function testAfterClass(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		for (expected in ["private", "extern", "class", "interface", "enum", "abstract", "typedef", "final"]) {
			assertHasCompletion(result, item -> item.kind == Keyword && item.args.name == expected);
		}
		for (unexpected in ["import", "using", "package"]) {
			assertHasNoCompletion(result, item -> item.kind == Keyword && item.args.name == unexpected);
		}
	}

	/**
		class C { }
		i{-1-}
	**/
	function testAfterClassIdent(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		for (expected in ["private", "extern", "class", "interface", "enum", "abstract", "typedef", "final"]) {
			assertHasCompletion(result, item -> item.kind == Keyword && item.args.name == expected);
		}
		for (unexpected in ["import", "using", "package"]) {
			assertHasNoCompletion(result, item -> item.kind == Keyword && item.args.name == unexpected);
		}
	}

	/**
		{-1-}class
	**/
	function testOnClass(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		for (expected in ["import", "using", "private", "extern", "class", "interface", "enum", "abstract", "typedef", "final"]) {
			assertHasCompletion(result, item -> item.kind == Keyword && item.args.name == expected);
		}
	}

	/**
		private {-1-}
	**/
	function testAfterPrivate(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		for (expected in ["extern", "class", "interface", "enum", "abstract", "typedef", "final"]) {
			assertHasCompletion(result, item -> item.kind == Keyword && item.args.name == expected);
		}
		assertHasNoCompletion(result, item -> item.kind == Keyword && item.args.name == "private");
	}

	/**
		extern {-1-}
	**/
	function testAfterExtern(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		for (expected in ["private", "class", "interface", "enum", "abstract", "typedef", "final"]) {
			assertHasCompletion(result, item -> item.kind == Keyword && item.args.name == expected);
		}
		assertHasNoCompletion(result, item -> item.kind == Keyword && item.args.name == "extern");
	}

	/**
		final {-1-}
	**/
	function testAfterFinal(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		for (expected in ["private", "class", "interface", "extern"]) {
			assertHasCompletion(result, item -> item.kind == Keyword && item.args.name == expected);
		}
		for (unexpected in ["final", "enum", "typedef", "abstract"]) {
			assertHasNoCompletion(result, item -> item.kind == Keyword && item.args.name == unexpected);
		}
	}

	/**
		final extern {-1-}
	**/
	function testAfterFinalExtern(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		for (expected in ["private", "class", "interface"]) {
			assertHasCompletion(result, item -> item.kind == Keyword && item.args.name == expected);
		}
		for (unexpected in ["final", "enum", "typedef", "abstract", "extern"]) {
			assertHasNoCompletion(result, item -> item.kind == Keyword && item.args.name == unexpected);
		}
	}

	/**
		package;
		{-1-}
	**/
	function testInImportHx(_) {
		vfs.putContent("import.hx", markers.source);
		vfs.putContent("Main.hx", "");
		runHaxeJson([], DisplayMethods.Completion, {file: new FsPath("import.hx"), offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> item.kind == Keyword && item.args.name == "import");
		assertHasCompletion(result, item -> item.kind == Keyword && item.args.name == "using");
		Assert.equals(2, result.result.items.length);
	}
}
