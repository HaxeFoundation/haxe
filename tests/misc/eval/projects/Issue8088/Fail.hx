class Fail {
	static function main() {}

	static function check<T:String>(a:T, b:Float) {
		a < b;
		a - b;
		a / b;
		b < a;
		b - a;
		b / a;
	}
}

abstract AStr(String) to String {}