package unit.issues;

class Issue5030 extends Test {
	function test() {
		t(isX());
	}

	@:analyzer(no_const_propagation)
	function isX() {
		var input:Input = { axis: X };
		var b:Bool = true;
		return (input.axis == X) ? b: false;
	}
}

enum XY {
	X;
	Y;
}

typedef Input = {
	var axis:XY;
}