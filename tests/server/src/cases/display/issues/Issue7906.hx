package cases.display.issues;

class Issue7906 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var method:Ha{-1-}xeRequestMethod;
				n{-2-}ew Ha{-3-}xeRequestMethod();
			}
		}

		typedef {-4-}HaxeRequestMethod{-5-} = RequestMethod;

		abstract RequestMethod(String) to String {
			public function {-6-}new{-7-}()
				this = "";
		}
	**/
	function test(_) {
		var locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(1)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(4, 5), locs[0].range);

		locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(2)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(6, 7), locs[0].range);

		locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(3)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(4, 5), locs[0].range);
	}
}
