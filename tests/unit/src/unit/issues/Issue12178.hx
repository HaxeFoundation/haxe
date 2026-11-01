package unit.issues;

@:struct private class Point {
	public var x : Int;
	public function new( x : Int ) { this.x = x; }
	public function toString() : String { return 'Point(x=$x)'; }
}

class Issue12178 extends Test {
	var count : Int = 10;
	#if hl
	function test() {
		var nativeArr = new hl.NativeArray<Point>(count);
		for( i in 0...count ) {
			nativeArr[i] = new Point(20+i);
		}
		Std.string(nativeArr);
		noAssert();
	}
	#end
}
