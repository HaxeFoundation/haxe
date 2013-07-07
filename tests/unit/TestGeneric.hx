package unit;

@:generic
class MyGeneric<T> {
	public var t:T;
	public function new(t:T) {
		this.t = t;
	}
}

@:generic
class MyGeneric2<T> extends T {
	//public function new() { } // not allowed
}

class MyRandomClass {
	public var s:String;
	public function new(s:String) {
		this.s = s;
	}
}

class TestGeneric extends Test {
	function testBasic() {
		var mg = new MyGeneric<Int>(12);
		eq(mg.t, 12);
		t(Std.is(mg.t, Int));
		
		var mg = new MyGeneric<String>("12");
		eq(mg.t,"12");
		t(Std.is(mg.t, String));
	}
	
	function testExtends() {
		t(unit.TestType.typeError(new MyGeneric2<String>()));
		t(unit.TestType.typeError(new MyGeneric2<Int>()));
		
		var mg = new MyGeneric2<MyRandomClass>("foo");
		eq("foo", mg.s);
		
		var mg = new MyGeneric2<MyGeneric<MyRandomClass>>(new MyRandomClass("foo"));
		eq("foo", mg.t.s);
	}
}