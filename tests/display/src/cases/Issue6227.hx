package cases;

class Issue6227 extends DisplayTestCase {
	/**
	class Main {
		public static function main() {
		new Foo(4) {-1-}> new Foo(3);
		}
	}

	abstract Foo({i:Int}) {
		public function new(i) this = {i: i};
		@:op(a > b) function f(other:Foo):String return "true";
	}
	**/
	function test() {
		eq("String", type(pos(1)));
	}
}