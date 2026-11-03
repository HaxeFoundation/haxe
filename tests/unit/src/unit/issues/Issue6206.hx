package unit.issues;

private class Foo {
	public var value:String;

	public function new(value:String) {
		this.value = value;
	}

	public function test() {
		return value;
	}
}

class Issue6206 extends unit.Test {
	#if !neko // neko binds the method to `o`
	function test() {
		eq("foo", Reflect.callMethod({}, new Foo("foo").test, []));
	}
	#end
}
