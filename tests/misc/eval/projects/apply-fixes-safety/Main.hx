class Old {
	public static function twice(x:Int, y:Int) {
		return x + y;
	}

	@:deprecated("use twice", Old.twice(x, x))
	public static function double(x:Int) {
		return x * 2;
	}

	@:deprecated("use zero", 0)
	public static function nothing(x:Int) {
		return 0;
	}
}

class Main {
	static var counter = 0;

	static function next() {
		return ++counter;
	}

	static function main() {
		var a = 1;
		trace(Old.double(a), Old.double(next()), Old.nothing(2), Old.nothing(next()));
	}
}
