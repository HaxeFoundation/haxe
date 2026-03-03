package cases.display.issues;

class Issue7158 extends DisplayTestCase {
	/**
		class {-2-}Main{-3-} {
			static function main() {
				var s:Mai{-1-}n<T>;
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(1)});
		var locs = parseGotoDefintion().result;
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(2, 3), locs[0].range);
	}
}
