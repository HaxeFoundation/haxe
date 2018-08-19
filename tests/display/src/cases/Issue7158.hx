package cases;

class Issue7158 extends DisplayTestCase {
	/**
	class {-2-}Main{-3-} {
		static function main() {
			var s:Mai{-1-}n<T>;
		}
	}
	**/
	function test() {
		eq(range(2, 3), position(pos(1)));
	}
}