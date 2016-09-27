package cases;

class Issue5171 extends DisplayTestCase {
	/**
	{-2-}class Main {
		static function main() {
			Ma{-1-}in;
		}
	}{-3-}
	**/
	function test() {
		eq(range(2, 3), position(pos(1)));
	}
}