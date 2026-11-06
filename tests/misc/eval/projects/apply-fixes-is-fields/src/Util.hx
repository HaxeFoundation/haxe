class Util {
	public static function is(v:Dynamic) {
		return v != null;
	}

	public static function check(v:Dynamic) {
		return is(v) && Util.is(v);
	}
}
