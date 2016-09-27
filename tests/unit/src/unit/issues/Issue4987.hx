package unit.issues;

class Issue4987 extends Test implements ISetter implements ISetter2 {
	public var property(default, set):Int;
	function set_property(i) return 0;

	function test() {
		property = 0;
	}
}

interface ISetter {
	var property(default, set):Int;
}

interface ISetter2 {
	var property(default, set):Int;
}