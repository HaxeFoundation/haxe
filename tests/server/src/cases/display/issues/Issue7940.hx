package cases.display.issues;

import haxe.display.Diagnostic;

class Issue7940 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				{-1-}"Hello World
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		Assert.equals(1, diags.length);
		Assert.equals(DKParserError, diags[0].kind);
		Assert.equals(Error, diags[0].severity);
		Assert.equals("Unterminated string", diags[0].args);
		Assert.equals(range(1, 1), diags[0].range);
	}
}
