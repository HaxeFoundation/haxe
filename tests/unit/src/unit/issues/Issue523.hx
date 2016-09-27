package unit.issues;

private class A extends Arr<B> {
	public function new() {
		super(new B(this));
	}
}

private class B {
	var arr:Arr<B>;

	public function new(arr:Arr<B>) {
		this.arr = arr;
	}

	public inline function getColor(strength:Int) {
		switch (arr.index) {
			case _: return strength;
		}
	}
}

private class Arr<T> {
	public var index:Int;
	public var v:T;

	public function new(v) {
		this.index = 0;
		this.v = v;
	}
}

class Issue523 extends Test {
	function test() {
		var a = new A();
		eq(142, a.v.getColor(142));
	}
}