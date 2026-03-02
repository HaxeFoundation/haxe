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
		var diag1 = diags.find(d -> d.kind == DKUnresolvedIdentifier);
		Assert.notNull(diag1);
		Assert.same(range(3, 4), diag1.range);
		var diag2 = diags.find(d -> d.kind == DKCompilerError && d.args == "Can only extend structures");
		Assert.notNull(diag2);
		Assert.same(range(1, 2), diag2.range);
	}
}
