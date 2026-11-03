@:native("test.MathOperation")
class MathOperation {
	public static inline overload function perform(op:Int->Int, value:Int) {
		return op(value);
	}

	public static inline overload function perform(op:Int->Int->Int, value:Int) {
		return op(value, value);
	}
}

class MainFail {
	static function main() {
		Sys.println(MathOperation.perform(double, 3));
		Sys.println(MathOperation.perform(multiply, 3));
	}

	static function double(a):Int {
		return a * 2;
	}

	static function multiply(a, b):Int {
		return a * b;
	}
}
