class Helper {
	public static var seven = 7;
}

class Main {
	// non-constant default for a basic value type: unsupported on flash
	static function f(x:Int = Helper.seven):Int return x;

	static function main() {
		f();
	}
}
