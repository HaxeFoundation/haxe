package cases;

/** Local functions, captured variables, higher-order calls. **/
class Closures {
	static function fold<T>(a:Array<T>, f:(Int, T) -> Int, init:Int) {
		var acc = init;
		for (x in a)
			acc = f(acc, x);
		return acc;
	}

	public static function run() {
		var a = [for (i in 0...1000) i];
		var sum = 0;
		for (k in 0...100) {
			var counter = 0;
			function incr(x:Int) {
				counter++;
				return x + k;
			}
			var b = a.map(incr).filter(x -> x % 2 == 0);
			sum = fold(b, (acc, x) -> (acc + x) & 0xFFFFFF, sum);
			sum = (sum + counter) & 0xFFFFFF;
		}
		return sum;
	}
}
