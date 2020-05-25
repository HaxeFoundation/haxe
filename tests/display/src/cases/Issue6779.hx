package cases;

class Issue6779 extends DisplayTestCase {
	/**
		class Some {
			static function f():Void {}
		}

		class Main {
			static function main() {
				@:privateAccess Some.{-1-}
			}
		}
	**/
	function test() {
		eq(true, hasField(fields(pos(1)), "f", "() -> Void", "method"));
	}
}
