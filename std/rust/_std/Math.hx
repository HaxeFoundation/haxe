class Math {
	public static function floor(v:Float):Float {
		return __rust__("f32.floor(v)");
	}
}