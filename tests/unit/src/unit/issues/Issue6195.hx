package unit.issues;

class Issue6195 extends unit.Test {
	var field(get,default):Int = 1;
	function get_field():Int {
		return field + 1;
	}

	function test() {
		eq(2, field);
		field = 10;
		eq(11, field);
	}
}