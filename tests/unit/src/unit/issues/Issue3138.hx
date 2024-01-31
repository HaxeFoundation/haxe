package unit.issues;

class Issue3138 extends Test
{
	public function test()
	{
		var a = new B();
#if java
		var b = new D();
#end
		noAssert();
	}
}

private class A {
	public function new(_) {}
}

private class B extends A {
	public function new(?a = 1) {
		super(a); // error CS0030: Cannot convert type 'int' to 'haxe.lang.Null<int>'
	}
}

#if java

private class C {
	public function new(a) {}
}

private class D extends C {
	public function new(?a:Single = 1) {
		super(a); // error CS0030: Cannot convert type 'int' to 'haxe.lang.Null<int>'
	}
}

#end
