package unit.issues;

private class Foo {
	public var m : Map<Int, String>;
	public var length : Int;
	public function new() {
	}
}

class Issue12000 extends unit.Test {
	var obj = new Foo();

	#if hl
	public function test() {
		untyped $prefetch(obj, 0);
		untyped $prefetch(obj, 1);
		untyped $prefetch(obj, 2);
		untyped $prefetch(obj, 3);
		untyped $prefetch(obj, 4);
		untyped $prefetch(obj.length, 0);
		untyped $prefetch(obj.length, 1);
		untyped $prefetch(obj.length, 2);
		untyped $prefetch(obj.length, 3);
		untyped $prefetch(obj.length, 4);
		noAssert();
	}
	#end
}
