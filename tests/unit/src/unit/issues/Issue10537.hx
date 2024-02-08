package unit.issues;

import haxe.macro.Context;

class Issue10537 extends Test {
	macro static function m() {
		var imports = [
			"haxe.ds.StringMap",
			"StringTools.endsWith",
			"StringTools.*",
			"StringTools.startsWith as sw"
		];
		var usings = ["StringTools"];
		Context.withImports(imports, usings, () -> {
			Context.typeExpr(macro {
				StringMap;
				endsWith("foo", "o");
				startsWith("foo", "o");
				sw("foo", "f");
				"foo".endsWith("o");
			});
		});

		// nested
		Context.withImports(["haxe.ds.StringMap"], [], () -> {
			Context.withImports(["StringTools.endsWith"], [], () -> {
				Context.typeExpr(macro {
					StringMap;
					endsWith("foo", "o");
				});
			});
		});
		return macro null;
	}

	function test() {
		m();
		utest.Assert.pass();
	}
}
