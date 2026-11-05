class Old {
	@:deprecated("use future", Old.future(x), "6.0.0")
	public static function next(x:Int) return x;

	@:deprecated("use now", Old.now(x))
	public static function before(x:Int) return x;

	public static function future(x:Int) return x;

	public static function now(x:Int) return x;
}

class Main {
	static function main() {
		trace(Old.next(1), Old.before(2));
	}
}
