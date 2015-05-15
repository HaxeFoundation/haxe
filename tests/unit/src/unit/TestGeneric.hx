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

class MyRandomEmptyClass { }

@:generic class RBNode<T:RBNode<T>> {
	public var rbLeft : T;
	public var rbRight : T;
}

@:generic class RBTree<T:RBNode<T>> {
	public var root : T;
	public function new() {	}
}

class MyData extends RBNode<MyData> {
	var id : Int;
	public function new(id:Int) {
		this.id = id;
	}
}

class TestGeneric extends Test {
	function testBasic() {
		var mg = new MyGeneric<Int>(12);
		eq(mg.t, 12);
		t((mg.t is Int));

		var mg = new MyGeneric<String>("12");
		eq(mg.t,"12");
		t((mg.t is String));
	}

	function testExtends() {
		// basic class
		//t(unit.TestType.typeError(new MyGeneric2<String>()));

		// not a class
		//t(unit.TestType.typeError(new MyGeneric2<Int>()));

		// no constructor
		//t(unit.TestType.typeError(new MyGeneric2<MyRandomEmptyClass>()));

		var mg = new MyGeneric2<MyRandomClass>("foo");
		eq("foo", mg.s);

		var mg = new MyGeneric2<MyGeneric<MyRandomClass>>(new MyRandomClass("foo"));
		eq("foo", mg.t.s);
	}

	function testConstraints() {
		var n = new RBTree<MyData>();
		n.root = new MyData(1);
		n.root.rbLeft = new MyData(2);
		n.root.rbRight = new MyData(3);
	}
}