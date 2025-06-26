package cases;

class Issue9084 extends DisplayTestCase {
	/**
		class A {
			public function {-2-}new{-3-}() {}
		}

	 	class Main {
			static function main() {
				A.n{-1-}ew;
			}
		}
	**/
	function test() {
		eq(range(2, 3), position(pos(1)));
	}
}
