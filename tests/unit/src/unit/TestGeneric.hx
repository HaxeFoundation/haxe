package unit;

private typedef MyAnon = {
	a:Int,
	?b:MyRandomClass,
}

@:generic
class MyGeneric<T> {
	public var t:T;
	public function new(t:T) {
		this.t = t;
	}
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

	function testConstraints() {
		var n = new RBTree<MyData>();
		n.root = new MyData(1);
		n.root.rbLeft = new MyData(2);
		n.root.rbRight = new MyData(3);
		noAssert();
	}

	function testGenericAnon() {
		var a = new MyGeneric<MyAnon>({a: 1});
		eq(a.t.a, 1);
		eq(a.t.b, null);
	}

	function testGenericFn() {
		var a = new MyGeneric<Int->Int>(function(i:Int):Int return i * i);
		eq(4, a.t(2));
	}
}
