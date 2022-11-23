package unit.issues;

class Issue10834 extends Test {
	var tmp:Any;
	var a:Dynamic = [];

	function test() {
		tmp = a[0];
		eq(0, a.length);
		eq('[]', a.toString());
	}
}