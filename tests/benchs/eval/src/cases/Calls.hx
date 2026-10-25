package cases;

/** Calls of static functions and integer arithmetic. **/
class Calls {
	static function fib(n:Int):Int {
		return n < 2 ? n : fib(n - 1) + fib(n - 2);
	}

	static function add3(a:Int, b:Int, c:Int):Int {
		return a + b + c;
	}

	public static function run() {
		var sum = fib(25);
		for (i in 0...300000)
			sum = add3(sum, i, 1) & 0xFFFFFF;
		return sum;
	}
}
