package unit.issues;

class Issue2607 extends unit.Test {

	inline static var CONST:Float = -1;

	function fun(v = CONST) {
		eq(v, -1);
		t((v is Float));
	}

	function test() {
		fun();
	}
}