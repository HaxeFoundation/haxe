package unit.issues;

class Issue5543 extends Test {
	function test() {
		f1({ i: 5 });
	}

	static function f1(n:NullableInt) {
		f2(n.i);
	}

	static function f2(i:Int = 0) {}
}

typedef NullableInt = {
	?i:Int
}