package unit.issues;

class Issue5556 extends unit.Test {
	function test() {
		var x = dynamicFunc(randomCall());
		eq(1, x);
	}

	static function randomCall() {
		dynamicFunc = function(x:Int) return x * 2;
		return 1;
	}

	static dynamic function dynamicFunc(x) return x;
}