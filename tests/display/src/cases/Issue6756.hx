package cases;

class Issue6756 extends DisplayTestCase {
	/**
		abstract Result(String) {
			function f{-1-}oo() {}
		}
	**/
	function test() {
		eq("() -> Void", type(pos(1)));
	}
}
