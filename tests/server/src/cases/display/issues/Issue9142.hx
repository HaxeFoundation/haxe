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
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("String", result.item.type.args.path.typeName);
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
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("String", result.item.type.args.path.typeName);
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
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("String", result.item.type.args.path.typeName);
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
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("String", result.item.type.args.path.typeName);
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
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("String", result.item.type.args.path.typeName);
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
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("String", result.item.type.args.path.typeName);
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
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("String", result.item.type.args.path.typeName);
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
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("String", result.item.type.args.path.typeName);
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
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("String", result.item.type.args.path.typeName);
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
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("String", result.item.type.args.path.typeName);
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
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("String", result.item.type.args.path.typeName);
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
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("String", result.item.type.args.path.typeName);
	}
}
