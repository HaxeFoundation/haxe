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
}