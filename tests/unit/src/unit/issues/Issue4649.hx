package unit.issues;

class Issue4649 extends unit.Test {
	static var v:Val<Float> = 10.5;
#if !cpp
	function test() {
		v.incr();
		eq(11.5, v);
	}
#end
}

private abstract Val<T:Float>(T) from T to T {
	public inline function incr():Void
		this++;
}