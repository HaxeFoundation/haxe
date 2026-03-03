package cases.display.issues;

import haxe.display.Diagnostic;

class Issue7948 extends DisplayTestCase {
	/**
		class Main {
			{-1-}class{-2-} Moin {

			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		Assert.equals(1, diags.length);
		Assert.isTrue(diags[0].kind == DKParserError);
		Assert.equals(Error, diags[0].severity);
		Assert.equals('Unexpected keyword "class"', diags[0].args);
		Assert.same(range(1, 2), diags[0].range);
	}

	/**
		class Main {
			static function main()
				trace("Test");
			}
		{-1-}}{-2-}
	**/
	function test2(_) {
		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		Assert.equals(1, diags.length);
		Assert.isTrue(diags[0].kind == DKParserError);
		Assert.equals(Error, diags[0].severity);
		Assert.equals("Unexpected }", diags[0].args);
		Assert.same(range(1, 2), diags[0].range);
	}
}
