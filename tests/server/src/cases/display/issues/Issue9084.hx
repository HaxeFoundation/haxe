package cases.display.issues;

class Issue9084 extends DisplayTestCase {
	/**
		class A {
			public function {-2-}new{-3-}() {}
		}

	 	class Main {
			static function main() {
				A.n{-1-}ew;
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
