package cases;

class Issue5171 extends DisplayTestCase {
	/**
	class {-2-}Main{-3-} {
		static function main() {
			Ma{-1-}in;
		}
	}
	**/
	function test() {
		eq(range(2, 3), position(pos(1)));
	}
}