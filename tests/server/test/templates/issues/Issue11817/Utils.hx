class Utils {
	public static function foo():Int {
		static var leak = 0;
		return leak;
	}
}
