package unit.issues;

private abstract Abstract(Dynamic) {
	@:from public static inline function fromObject(v:Dynamic):Abstract
		return cast v;

	@:to public inline function toInt():Int
		return Std.int(this);

	@:to public inline function toFloat():Float
		return this;
}

class Issue8389 extends unit.Test {
	function test() {
		var a:Abstract = 0.5;
		var f:Float = a;
		feq(0.5, f);
		var i:Int = a;
		eq(0, i);
	}
}
