package unit.issues;

private class GenericTest<T:haxe.Constraints.Constructible<String->Void>> {
	public function new(){}

	@:generic
	public function getInstance<S:T>(arg:String):S {
		return new S(arg);
	}
}

class Issue4668 extends Test {
	function test() {
		var gen = new GenericTest<haxe.Template>();
		var t = gen.getInstance("my ::template::");
		eq("my foo", t.execute({template: "foo"}));

		var gen = new GenericTest<String>();
		var t = gen.getInstance("my string");
		eq("my string", t);
	}
}