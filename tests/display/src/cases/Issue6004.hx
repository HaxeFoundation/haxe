package cases;

class Issue6004 extends DisplayTestCase {
	/**
	class Main {
		static function f(a:Int) return a;

		static function main() {
			f.{-1-}
		}
	}
	**/
	function test() {
		eq(true, hasField(fields(pos(1)), "bind", "(?a : Int -> Int) -> (?a : Int -> Int)", "method"));
	}
}
