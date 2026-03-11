package cases.display.issues;

class Issue7133 extends DisplayTestCase {
	/**
		import haxe.ds.Option;

		class Main {
			public static function main() {
				var option:Option<Int> = if (true) No{-1-}ne else
			}
		}
	**/
	function test(_) {
		var locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(1)});
		Assert.isTrue(locs != null && locs.length > 0);
	}
}
