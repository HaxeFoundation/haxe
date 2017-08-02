package cases;

class Issue6417 extends DisplayTestCase {
	/**
	class Main {
		static function main() {}

		macro function foo({-1-}body{-2-}:Expr) {
			macro function() $bo{-3-}dy;
		}
	}
	**/
	function test() {
		eq(range(1, 2), position(pos(3)));
		eq("Dynamic", type(pos(3)));
	}
}
