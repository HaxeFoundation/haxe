package cases;

class Issue8420 extends DisplayTestCase {
	/**
		class A {
			final x:Int;

			function f() {
				this.{-1-}
			}
		}
	**/
	function test() {
		eq(true, hasField(fields(pos(1)), "x", "Int"));
	}
}
