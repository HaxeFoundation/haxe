package unit.issues;

class Issue3949 extends unit.Test {
#if cs
	function test() {
		var a = new Arr<Issue3949>();
		a.arr[0] = this;
		checkCastClass(a.arr[0]);

		var a = new Arr<Int>();
		a.arr[0] = 10;
		checkCastInt(10, a.arr[0]);

		var a = new Arr<String>();
		a.arr[0] = 'hello';
		checkCastString('hello', a.arr[0]);
	}

	@:pure(false)
	function checkCastClass(v:Issue3949) {
		eq(this, v);
	}

	@:pure(false)
	function checkCastInt(v1:Int, v2:Int) {
		eq(v1, v2);
	}

	@:pure(false)
	function checkCastString(v1:String, v2:String) {
		eq(v1, v2);
	}
#end
}
#if cs
private class Arr<T> {
	public var arr = new cs.NativeArray<T>(1);
	public function new() {}
}
#end