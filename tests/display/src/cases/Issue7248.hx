package cases;

class Issue7248 extends DisplayTestCase {
	/**
		abstract Foo(Int) {
			static public function AStatic(){}
			public function NonStatic(){}
		}
		class Main {
			static function main() {
				Foo.{-1-}
			}
		}
	**/
	function test() {
		eq(true, hasField(fields(pos(1)), "AStatic", "() -> Void"));
		eq(false, hasField(fields(pos(1)), "NonStatic", "this : Int -> Void"));
	}
}
