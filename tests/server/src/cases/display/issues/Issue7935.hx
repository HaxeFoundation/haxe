package cases.display.issues;

import haxe.display.Diagnostic;

class Issue7935 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {
				trace({-1-};{-2-}
			}
		}
	**/
	function test1(_) {
		hasParserError("Expected expression or )");
	}

	/**
		class Main {
			public static function main() {
				trace(0
			{-1-}}{-2-}
		}
	**/
	function test2(_) {
		hasParserError("Expected , or )");
	}

	/**
		class Main {
			public static function main() {
				trace(0,
			{-1-}}{-2-}
		}
	**/
	function test3(_) {
		hasParserError("Expected expression");
	}

	function hasParserError(message:String) {
		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		Assert.isTrue(diags.exists(d -> d.kind == DKParserError && d.args == message && d.range == range(1, 2)));
	}
}
