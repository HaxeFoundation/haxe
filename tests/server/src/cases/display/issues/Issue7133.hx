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
		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(1)});
		var locs = parseGotoDefintion().result;
		Assert.isTrue(locs != null && locs.length > 0);
	}
}
