package cases;

class Issue12254 extends DisplayTestCase {
	/**
		using Issue12254.Tools;

		class C {
			public function new() {}
			@:noCompletion public function f() {}
		}

		class Tools {
			static public function f(c:C, s:String) {}
		}

		function main() {
			var c = new C();
			c.{-1-}
		}
	**/
	function test() {
		final fields = fields(pos(1));
		eq(0, fields.length);
	}
}
