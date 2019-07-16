package unit.issues;

class Issue4649 extends unit.Test {
	static var v:Val<Float> = 10.5;
	function test() {
		v.incr();
		eq(11.5, v);
	}
}

private abstract Val<T:Float>(T) from T to T {
	public inline function incr():Void
		this++;
}