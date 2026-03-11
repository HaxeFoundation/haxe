package cases.display.issues;

class Issue7111 extends DisplayTestCase {
	/**
		class {-1-}Main{-2-}<T> {
			public static function main() {
				var a:Ma{-3-}in<>
			}
		}
	**/
	function test(_) {
		var locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(3)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(1, 2), locs[0].range);
	}
}
