class InlineClass {
	public var a = 1;
	public var b = "";
	public var c = "hello";
	public inline function new() {
	}

	public inline function method() {
		return a + b + c;
	}

	public inline function cancelThis() {
		noInline(this);
		return [1,2];
	}

	// Used to ensure the cancelation of inlining of the object passed as argument
	public static function noInline(a : InlineClass) {
	}
}

class InlineIterator {
	public var i = 0;
	public inline function new() {};
	public inline function hasNext() return i < 10;
	public inline function next() return i++;
}

class NestedInlineClass {
	public var a : InlineClass;
	public var b : Array<Int>;
	public var c : {a: Int};

	public inline function new() {
		a = new InlineClass();
		b = [1,2,3];
		c = {a:4};
	}
}

class TestInlineConstructors extends TestBase {
	@:js('return [1,2,3,3];')
	static function testArrayInlining() {
		var a = [1,2,3];
		return [a[0], a[1], a[2], a.length];
	}

	@:js('return [2,"hello","world"];')
	static function testAnonymousStructureInlining() {
		var a = {a: 1, b: "", c: "hello"};
		a.a = 2;
		a.b = a.c;
		a.c = "world";
		return ([a.a,a.b,a.c] : Array<Dynamic>);
	}

	@:js('return [2,"hello","world"];')
	static function testClassConstructorInlining() {
		var a = new InlineClass();
		a.a = 2;
		a.b = a.c;
		a.c = "world";
		return return ([a.a,a.b,a.c] : Array<Dynamic>);
	}

	@:js('return [1,2,4,"test",2,1,4,3,4];')
	static function testNestedInlining() {
		var a = {
			a: new NestedInlineClass(),
			b: [new NestedInlineClass()],
			c: {test: new NestedInlineClass()}
		};
		a.b[0].a.c = "test";
		return ([a.a.a.a, a.a.b[1], a.a.c.a, a.b[0].a.c, a.b[0].b[1], a.b.length, a.c.test.c.a, a.c.test.b[2], a.c.test.c.a]:Dynamic);
	}

	@:js('return [1,"hello","world"];')
	static function testMultipleAliasingVariables() {
		var a = new InlineClass();
		var b = a;
		var d = [b,a];
		d[1].b = "hello";
		a.c = "world";
		return ([a.a, a.b, d[1].c]:Array<Dynamic>);
	}

	@:js('return [1,"","hello"];')
	static function testUnassignedVariables() {
		var a;
		a = new InlineClass();
		return ([a.a, a.b, a.c]:Array<Dynamic>);
	}

	@:js('return [1,3,"hello"];')
	static function testNoExplicitVariables() {
		return ([
			new InlineClass().a,
			[1,2,3][2],
			new NestedInlineClass().a.c
		]:Array<Dynamic>);
	}

	@:js('return 1 + "" + "hello";')
	static function testMethodInlining() {
		var a : {function method() : String;} = new InlineClass();
		return a.method();
	}

	@:js('var b = new InlineClass();return [new InlineClass().method(1),b.method(2)];')
	static function testIncompatibleMethodCancelling() {
		// InlineClass.method doesn't take any arguments, the method should not be inlined.
		var a : {function method(arg : Int) : String;} = cast new InlineClass();
		var b : Dynamic = new InlineClass();
		return [a.method(1), b.method(2)];
	}

	@:js('var arr = new InlineClass().cancelThis();return [arr[0],arr[1]];')
	static function testCancelOfReturnedObject() {
		var a : {function cancelThis() : Array<Int>;} = new InlineClass();
		var arr = a.cancelThis();
		return [arr[0], arr[1]];
	}

	@:js('var v_i = 0;var acc = 0;while(v_i < 10) acc += v_i++;return acc;')
	static function testIteratorMethodInliningInForLoop() {
		var iter : Iterator<Int> = new InlineIterator();
		var acc = 0;
		for ( v in iter ) {
			acc += v;
		}
		return acc;
	}
}
