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
		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		Assert.isTrue(diags.exists(d -> d.kind == DKParserError && d.args == "Expected expression or )" && d.range == range(1, 2)));
	}

	/**
		class Main {
			public static function main() {
				trace(0
			{-1-}}{-2-}
		}
	**/
	function test2(_) {
		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		Assert.isTrue(diags.exists(d -> d.kind == DKParserError && d.args == "Expected , or )" && d.range == range(1, 2)));
	}

	/**
		class Main {
			public static function main() {
				trace(0,
			{-1-}}{-2-}
		}
	**/
	function test3(_) {
		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		Assert.isTrue(diags.exists(d -> d.kind == DKParserError && d.args == "Expected expression" && d.range == range(1, 2)));
	}
}
