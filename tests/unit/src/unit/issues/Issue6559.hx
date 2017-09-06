package unit.issues;

class Issue6559 extends unit.Test {
	function test() {
		var value = Reflect.getProperty(this, 'nonExistentField');
		t(value == null);
	}
}