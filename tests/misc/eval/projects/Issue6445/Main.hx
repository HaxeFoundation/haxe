class Main {
	static function main() {
		var e:Int = f();
	}

	static function f() {
		return 1 + false; // type error is here
	}
}