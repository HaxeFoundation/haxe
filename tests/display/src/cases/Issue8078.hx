package cases;

class Issue8078 extends DisplayTestCase {
	/**
		extern class StringBuilder {
			@:overload(function new(s:String) {})
			public function new() { }

			public function append():Void { }
		}
		class Main {
			static function main() {
				var b = new StringBuilder("hi");
				b.{-1-}
			}
		}
	**/
	function test() {
		eq(true, hasField(fields(pos(1)), "append", "() -> Void"));
	}
}
