package unit;

class Box {
	var value:Dynamic;

	public function new(value:Dynamic) {
		this.value = value;
	}

	public function get<T>():T {
		return value;
	}
}

interface IFoo {}

class Foo implements IFoo {
	public function new() {}
}

class Bar {
	public var val:Int;
}

class TestHL extends Test {
	function testRetTypeTP() {
		var box = new Box(new Foo());
		try {
			var bar:Bar = box.get();
			trace(bar.val);
			assert("Expected cast failure");
		} catch(e: haxe.Exception) {
			eq(e.message, "Can't cast unit.Foo to unit.Bar");
		}
	}

	function testRetTypeTPIFace() {
		var foo = new Foo();
		var ifoo: IFoo = foo;
		var box = new Box(ifoo);
		var bar:Foo = box.get();
		// important: eq must not be used as casting the values to dynamic papers over some issues
		t(foo == bar);
	}

	//private function refTest(i:hl.types.Ref<Int>):Void
	//{
		//i *= 2;
	//}

	private function refTestAssign(i:hl.Ref<Int>):Void
	{
		i.set(2);
	}

	public function testRef()
	{
		var i = 10;
		refTestAssign(i);
		eq(i, 2);

		//var i = 10;
		//refTest(i);
		//eq(i, 20);
	}
}