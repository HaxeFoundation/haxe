package ;

class TestTreBehavior extends TestBase {

	static function main() {
		new TestTreBehavior();
	}

	public function new() {
		super();
		TestBaseMacro.run();
	}

	function testClosureCapturedArgs() {
		var steps = [];

		function loop(a:Int):Int {
			steps.push(() -> a);
			--a;
			return a <= 0 ? 0 : loop(a - 1);
		}
		loop(5);

		var actual = steps.map(fn -> fn());
		switch actual {
			case [4, 2, 0]:
			case _: assertEquals(actual, [4, 2, 0]);
		}
	}

	function testOverriddenMethod() {
		var parent = new Parent();
		var child = new Child();

		assertEquals(2, parent.rec(2));
		assertEquals(5, child.rec(2));
	}

	function testSelfModifyingFields() {
		assertEquals(1, selfModifyingMethod());
		assertEquals(2, selfModifyingVar());
	}

	static dynamic function selfModifyingMethod():Int {
		selfModifyingMethod = () -> 1;
		return selfModifyingMethod();
	}

	static var selfModifyingVar:()->Int = function() {
		selfModifyingVar = () -> 2;
		return selfModifyingVar();
	}
}

private class Parent {
	public function new() {}

	public function rec(n:Int, cnt:Int = 0):Int {
		if(n <= 0) return cnt;
		return rec(n - 1, cnt + 1);
	}
}

private class Child extends Parent {
	override public function rec(n:Int, cnt:Int = 0):Int {
		return super.rec(n, cnt + 1);
	}
}