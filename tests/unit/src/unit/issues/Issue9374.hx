package unit.issues;

class Issue9374 extends unit.Test {
	function test() {
		eq(123, clash(321));
	}

	@:analyzer(no_local_dce)
	function clash(name:Int):Int {
		var name = 123;
		return name;
	}
}