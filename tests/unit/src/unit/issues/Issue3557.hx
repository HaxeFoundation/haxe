package unit.issues;

private class MyBaseClass {
	public function new():Void {}
}

private class MyClass extends Generic<MyBaseClass> {
	public function new():Void {
		super();
	}
}

private interface Interface {}

@:generic
private class Generic<Default:{ public function new():Void; }>
	extends Default
	implements Interface {}

class Issue3557 extends Test {
	function test() {
		new MyBaseClass();
		new Generic<MyBaseClass>();
		new MyClass();
	}
}