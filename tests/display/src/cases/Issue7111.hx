package cases;

class Issue7111 extends DisplayTestCase {
	/**
		class {-1-}Main{-2-}<T> {
			public static function main() {
				var a:Ma{-3-}in<>
			}
		}
	**/
	function test() {
		eq(range(1, 2), position(pos(3)));
	}
}
