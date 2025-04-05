package unit.issues;

private abstract A(Int) from Int {
	@:to function toInt():Int return this * 2;
	@:op(a+b) static function f(a:A, b:A):A return 10;
}

class Issue8427 extends Test {
	function test() {
		var x = 10;
		x = x + (10 : A);
		eq(20, x);
		x += (10 : A);
		eq(20, x);
	}
}
