package cases;

using Lambda;

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
	function test() {
		var d = diagnostics();
		var range = diagnosticsRange(pos(1), pos(2));
		utest.Assert.isTrue(d.exists(d -> d.kind == MissingFields && utest.Assert.same(d.range, range)));
	}
}
