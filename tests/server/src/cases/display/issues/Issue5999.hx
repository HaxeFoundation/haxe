package cases.display.issues;

class Issue5999 extends DisplayTestCase {
	/**
		class Main {
			static inline var {-1-}value{-2-} = 1;

			static public function main() {
				trace(val{-3-}ue);
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(3)});
		var locs = parseGotoDefintion().result;
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(1, 2), locs[0].range);
	}
}
