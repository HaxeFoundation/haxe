package cases.display.issues;

class Issue6422 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				pro{-1-}perty;
			}

			static var {-2-}property{-3-}(get, set):Int;
			static function get_property() return 0;
			static function set_property(i) return 0;
		}
	**/
	function test(_) {
		var locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(1)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(2, 3), locs[0].range);
	}
}
