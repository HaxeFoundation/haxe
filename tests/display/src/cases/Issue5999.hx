package cases;

class Issue5999 extends DisplayTestCase {
	/**
	class Main {
		{-1-}static inline var value = 1;{-2-}

		static public function main() {
			trace(val{-3-}ue);
		}
	}
	**/
	function test() {
		eq(range(1, 2), position(pos(3)));
	}
}
