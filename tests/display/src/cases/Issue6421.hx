package cases;

class Issue6421 extends DisplayTestCase {
	/**
		using cases.Issue6421.Abstract;

		abstract Abstract(Int) {
			public function new(i) this = i;
			public function foo():Void { }
		}

		class Main {
			static function main() {
				0.{-1-}
			}
		}
	**/
	function test() {
		eq(false, hasField(fields(pos(1)), "foo", "() -> Void"));
	}
}
