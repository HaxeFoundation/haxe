package unit.issues;

@:generic
private class Gen<T> {
    public function new(t:T) { }
}

class Issue2044 extends Test {
	function test() {
		eq("unit.issues._Issue2044.Gen_String", Type.getClassName(Type.getClass(new Gen("foo"))));
		eq("unit.issues._Issue2044.Gen_Int", Type.getClassName(Type.getClass(new Gen(12))));
		eq("unit.issues._Issue2044.Gen_Bool", Type.getClassName(Type.getClass(new Gen(true))));
	}
}