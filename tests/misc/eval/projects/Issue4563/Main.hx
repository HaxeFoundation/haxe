class Main {
	static function main() {
		Sys.stderr().writeString("" + test(get(true)));
		Sys.stderr().writeString("" + test(get(false)));
	}

	static function get(b:Bool) return b;

	static inline function test(a:Bool):Int {
		if (a)
			return 42;
		else
			return 43;
	}
}