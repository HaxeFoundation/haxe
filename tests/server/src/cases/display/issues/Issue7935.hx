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
		var diag = diags.find(d -> d.kind == DKParserError && d.args == "Expected expression or )");
		Assert.notNull(diag);
		Assert.same(range(1, 2), diag.range);
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
		var diag = diags.find(d -> d.kind == DKParserError && d.args == "Expected , or )");
		Assert.notNull(diag);
		Assert.same(range(1, 2), diag.range);
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
		var diag = diags.find(d -> d.kind == DKParserError && d.args == "Expected expression");
		Assert.notNull(diag);
		Assert.same(range(1, 2), diag.range);
	}
}
