package unit.issues;

class Issue8538 extends unit.Test {
	function test() {
		var a:Null<Float> = 0;
		t(a == 0);
		t(0 == a);
	}
}