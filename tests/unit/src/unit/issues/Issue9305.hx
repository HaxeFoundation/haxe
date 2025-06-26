package unit.issues;

class Issue9305 extends unit.Test {
	function test() {
		var a = 999;
		var b = function() {
			a = 123;
			return a;
		}
		var r = add(a, b());
		eq(1122, r);
	}

	static function add(arg1:Int, arg2:Int) {
		return arg1 + arg2;
	}
}