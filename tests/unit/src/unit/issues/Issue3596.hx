package unit.issues;

private class ClassWithInitButNoConstructor {
	public var a = 1;
}

private class ClassWithInitButNoConstructorChild extends ClassWithInitButNoConstructor {
	public var b = 2;
}

//private class ClassWithInitAndConstructorChild extends ClassWithInitButNoConstructor {
	//public var b = 2;
	//public function new() {
		//super();
	//}
//}

private class ClassWithConstructor {
	public var a = 1;
	public function new() {
		a = 2;
	}
}

private class ClassWithConstructorChild extends ClassWithConstructor {
	public var b = 2;
}

private class ClassWithConstructorThatCalls {
	public var a = 1;
	public var b:Int;
	public function new() {
		init();
	}

	function init() {
		b = a;
	}
}

private class ClassWithConstructorThatCallsChild extends ClassWithConstructorThatCalls {
	public var c = 2;
	override function init() {
		b = c;
	}
}

class Issue3596 extends Test {
	function test() {
		t(unit.HelperMacros.typeError(new ClassWithInitButNoConstructor()));
		t(unit.HelperMacros.typeError(new ClassWithInitButNoConstructorChild()));

		//var c = new ClassWithInitAndConstructorChild();
		//eq(1, c.a);
		//eq(2, c.b);

		var c = new ClassWithConstructor();
		eq(2, c.a);

		var c = new ClassWithConstructorChild();
		eq(2, c.a);
		eq(2, c.b);

		var c = new ClassWithConstructorThatCalls();
		eq(1, c.a);
		eq(1, c.b);

		var c = new ClassWithConstructorThatCallsChild();
		eq(1, c.a);
		eq(2, c.b);
		eq(2, c.c);
	}
}