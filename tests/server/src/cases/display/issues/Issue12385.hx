package cases.display.issues;

import haxe.display.Diagnostic;

class Issue12385 extends DisplayTestCase {
	/**
		typedef Foo = {-1-}String{-2-} & {foo:Int};
		class Main {
			static function main() {
				{-3-}somebody{-4-};
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		Assert.isTrue(diags.exists(d -> d.kind == DKUnresolvedIdentifier && Std.string(d.range) == Std.string(range(3, 4))));
		Assert.isTrue(diags.exists(d -> d.kind == DKCompilerError && d.args == "Can only extend structures" && Std.string(d.range) == Std.string(range(1, 2))));
	}
}
