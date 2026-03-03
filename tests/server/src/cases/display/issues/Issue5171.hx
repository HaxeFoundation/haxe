package cases.display.issues;

class Issue5171 extends DisplayTestCase {
	/**
		class {-2-}Main{-3-} {
			static function main() {
				Ma{-1-}in;
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
