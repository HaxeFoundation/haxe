class Main {
	public static function main() {
		var a1:Array<Int> = [1, 2, 3];
		var a2:Array<Any> = a1;
		param(a1, a2);
	}

	static function param<T>(a1:T, a2:T) {}
}
