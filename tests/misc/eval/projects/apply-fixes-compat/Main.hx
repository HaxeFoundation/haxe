class Old {
	@:deprecated("use future")
	@:deprecated.replace(Old.future(x), "6.0.0")
	public static function next(x:Int) return x;

	@:deprecated("use now")
	@:deprecated.replace(Old.now(x))
	public static function before(x:Int) return x;

	@:deprecated("use future, or now before Haxe 6")
	@:deprecated.replace(Old.future(x), "6.0.0")
	@:deprecated.replace(Old.now(x))
	public static function either(x:Int) return x;

	public static function future(x:Int) return x;

	public static function now(x:Int) return x;
}

class Main {
	static function main() {
		trace(Old.next(1), Old.before(2), Old.either(3));
	}
}
