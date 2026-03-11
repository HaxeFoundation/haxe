package cases.display.issues;

import haxe.display.Diagnostic;

class Issue10713 extends DisplayTestCase {
	/**
		class Main {
		static function main() {
			loadEverything(() -> {
				{-1-}foo{-2-} = 1;
			});
		}

		static function loadEverything(cb:() -> Void, ?what):Void {}
		}
	**/
	function test(_) {
		var files = runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		final diags:Array<haxe.display.Diagnostic<Any>> = files != null && files.length > 0 ? cast files[0].diagnostics : [];
		var diag = diags.find(d -> d.kind == MissingFields);
		Assert.notNull(diag);
		Assert.same(range(1, 2), diag.range);
	}
}
