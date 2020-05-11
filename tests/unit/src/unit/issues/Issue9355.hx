package unit.issues;

class Issue9355 extends unit.Test {
	function test() {
		var o:Opacity = 0.5;
		eq('0.5', writeFloat(o));
	}

	static inline function writeFloat(f:Float)
		return Std.string(f);
}

private abstract Opacity(Float) from Float to Float {
	@:to public function toString():String
		return 'huh?';
} 