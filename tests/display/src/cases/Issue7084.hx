package cases;

class Issue7084 extends DisplayTestCase {
	/**
	enum abstract Foo(Int) {
		var Value = 0;
	}

	class Main {
		public static function main() {
			var f:Foo;
			f.{-1-}
		}
	}
	**/
	function test() {
		eq(0, fields(pos(1)).length);
	}
}