package cases.display.issues;

class Issue9142 extends DisplayTestCase {
	/**
		import NonExistent;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testNonExistentImport(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.equals("String", result.result.item.type.args.path.typeName);
	}

	/**
		import lowercase;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testLowercaseImport(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.equals("String", result.result.item.type.args.path.typeName);
	}

	/**
		import haxe.Int64.__Int64;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testPrivateImport(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.equals("String", result.result.item.type.args.path.typeName);
	}

	/**
		import StringTools as st;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testLowercaseAliasImport(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.equals("String", result.result.item.type.args.path.typeName);
	}

	/**
		import StringTools.NonExistent;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testNonExistentSubtypeImport(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.equals("String", result.result.item.type.args.path.typeName);
	}

	/**
		import StringTools.StringTools.NonExistent;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testNonExistentSubtypeFieldImport(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.equals("String", result.result.item.type.args.path.typeName);
	}

	/**
		import StringTools.StringTools.nonExistent;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testNonExistentSubtypeFieldImport2(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.equals("String", result.result.item.type.args.path.typeName);
	}

	/**
		import StringTools.StrongTools.NonExistent;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testNonExistentSubtypeFieldImport3(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.equals("String", result.result.item.type.args.path.typeName);
	}

	/**
		import StringTools.StrongTools.nonExistent;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testNonExistentSubtypeFieldImport4(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.equals("String", result.result.item.type.args.path.typeName);
	}

	/**
		import StringTools.StringTools.StringTools.StringTools;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testTooMuchImport(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.equals("String", result.result.item.type.args.path.typeName);
	}

	/**
		import StringTools.StringTools.StringTools.StringTools.*;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testTooMuchImportAll(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.equals("String", result.result.item.type.args.path.typeName);
	}

	/**
		import StringTools.StrongTools.*;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testNonExistentSubtypeAll(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.equals("String", result.result.item.type.args.path.typeName);
	}
}
