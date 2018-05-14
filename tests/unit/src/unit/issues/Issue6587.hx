package unit.issues;

class Issue6587 extends Test {
  static var consts = {
		friction: 0.5
	}

	function test() {
    f(0 == -consts.friction);
    var value = -consts.friction;
    eq(value, -0.5);
	}
}