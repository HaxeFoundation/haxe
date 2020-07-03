package unit.issues;

private class A<T:String> {
	public function new() f();
	function f():haxe.ds.Map<T,Int> throw new haxe.exceptions.NotImplementedException("Not implemented");
}

private class B extends A<String> {
	override function f() return ["a" => 1];
}

class Issue3777 extends unit.Test {
	function test() {
		// if it compiles, it works
		new B();
		noAssert();
	}
}
