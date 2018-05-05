package cases;

class Issue6943 extends DisplayTestCase {
	/**
class Main {
    public static function main() {}

    function foo(?{-1-}te{-2-}st{-3-}:Int) {}
}
	**/
	function test() {
		eq(range(1, 3), position(pos(2)));
	}
}
