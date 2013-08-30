class Test {
	public static function main() {
		var args = Sys.args();
		switch(args) {
			case ["fib", n]:
				var r:Int = 1, l:Int = 0;
				for(i in 1...Std.parseInt(n)) {
					r += i + l;
					l = r;
				}
				Sys.println('Fib($n) = $r');
			case [z]:
				Sys.println('Hello, $z!');
			default:
				throw 'Invalid arguments';
		}
	}
}