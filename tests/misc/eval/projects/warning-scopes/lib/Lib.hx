class Lib {
	public static function check(v:Dynamic) {
		return Std.is(v, Int);
	}

	@:deprecated("Use check instead")
	public static function old(v:Dynamic) {
		return check(v);
	}
}
