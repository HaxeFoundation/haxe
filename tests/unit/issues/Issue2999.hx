package unit.issues;

@:analyzer(no_check_has_effect)
class Issue2999 extends Test {
	function test1() {
		var a = [1, 2];
		eq(1, a[0]);
		eq(2, a[1]);
		a[-1];
	}

	function test2() {
		var a = [1, 2];
		eq(1, a[0]);
		eq(2, a[1]);
		a[2];
	}
}