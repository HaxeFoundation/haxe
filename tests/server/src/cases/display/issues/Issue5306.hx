package cases.display.issues;

import haxe.display.Diagnostic;

class Issue5306 extends DisplayTestCase {
	/**
		{-1-}import {-3-}InvalidType{-4-};{-2-}

		class Main {
			static function main() {
				var ib:Array<Int>;
				{-5-}ib{-6-}[0] = 0; ib[1] = 1; ib[2]
				{-7-}trace{-8-}("test");
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		Assert.isTrue(diags.exists(d -> d.kind == DKParserError && d.args == "Missing ;"));
		Assert.isTrue(diags.exists(d -> d.kind == DKCompilerError && (d.args:String).indexOf("InvalidType") != -1));
		Assert.isTrue(diags.exists(d -> d.kind == DKCompilerError && (d.args:String).indexOf("ib") != -1));
	}
}
