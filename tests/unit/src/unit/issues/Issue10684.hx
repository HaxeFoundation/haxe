package unit.issues;

class Issue10684 extends unit.Test {
	function test() {
		eq("1", makeMapF()([1 => "1", 2 => "2"]));
	}

	function makeMapF(?f:Map<Int, String>->String) {
		return f ?? v -> v[1];
	}
}
