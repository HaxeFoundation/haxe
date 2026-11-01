package unit.issues;

class Issue12921 extends Test {
#if jvm
	function test() {
		var c = new Issue12921_Canvas();
		var paint = new Issue12921_Paint();
		var left:Float = 1, top:Float = 2, right:Float = 3, bottom:Float = 4;
		var calls = 0;
		// The @:overload'd drawRect (Single overload) is called inside a closure
		// passed to an *inline* higher-order function. The leading Float (double)
		// arguments need a D2F conversion for the float overload. This used to emit
		// invalid bytecode (boxing the doubles to java.lang.Double instead of D2F),
		// producing a VerifyError at class load.
		withClip(() -> {
			c.drawRect(left, top, right, bottom, paint);
			calls++;
		});
		eq(1, calls);
	}

	static inline function withClip(body:Void->Void):Void {
		body();
	}
#end
}

#if jvm
private class Issue12921_Rect {
	public function new() {}
}

private class Issue12921_Paint {
	public function new() {}
}

private class Issue12921_Canvas {
	public function new() {}
	@:overload public function drawRect(rect:Issue12921_Rect, paint:Issue12921_Paint):Void {}
	@:overload public function drawRect(left:Single, top:Single, right:Single, bottom:Single, paint:Issue12921_Paint):Void {}
}
#end
