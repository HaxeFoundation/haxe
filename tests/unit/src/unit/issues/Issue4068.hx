package unit.issues;

class Issue4068 extends Test {
	function test() {
		eq(-30 % 100, -30);
		eq(0 % 100, 0);
		eq(-100 % 100, 0);
		eq(-30.0 % 100.0, -30.0);
		eq(30 % 100, 30);
		eq(30.0 % 100.0, 30.0);

		function i (x) return x;

		eq(i(-30) % i(100), i(-30));
		eq(i(0) % i(100), i(0));
		eq(i(-100) % i(100), i(0) );
		eq(i(30) % i(100), i(30));
	}

}