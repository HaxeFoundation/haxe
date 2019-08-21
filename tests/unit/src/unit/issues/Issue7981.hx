package unit.issues;

class Issue7981 extends unit.Test {
	function test() {
		var d:Dummy = 10;
		eq(12, d + 'ab');
		eq(13, d['abc']);
		eq(14, d.abcd);
	}
}

private abstract Dummy(Int) from Int {
	@:op(A + B) @:arrayAccess @:resolve
	public inline function resolve(key:String):Int
		return this + key.length;
}