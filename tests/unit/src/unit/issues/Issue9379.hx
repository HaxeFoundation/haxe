package unit.issues;

class Issue9379 extends unit.Test {
	function items() {}

	function test() {
		t(items != null);
		f(items == null);
		t(null != items);
		f(null == items);
	}
}