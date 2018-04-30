package cases;

class Issue6951 extends DisplayTestCase {
	/**
	class Main {
		public static function main() {
			new Test<"{-1-}">().foo();
		}
	}

	@:generic class Test<@:const T> {
		public function new() {}
		public function foo() {
			trace(T);
		}
	}
	**/
	function test() {
		eq("String", type(pos(1)));
	}
}
