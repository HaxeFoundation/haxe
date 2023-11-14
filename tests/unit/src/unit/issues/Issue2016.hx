package unit.issues;

@:generic
private class Gen<T> {
	public function new(a:T) {}
}

private typedef TGen<T> = Gen<T>;

class Issue2016 extends Test {
	function test() {
		var t = new TGen("a");
		eq("unit.issues._Issue2016.Gen_String", Type.getClassName(Type.getClass(t)));
		var t = new TGen(1);
		eq("unit.issues._Issue2016.Gen_Int", Type.getClassName(Type.getClass(t)));

		var t = new TGen<String>("a");
		eq("unit.issues._Issue2016.Gen_String", Type.getClassName(Type.getClass(t)));
	}
}
