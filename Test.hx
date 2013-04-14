import rust.*;
class Test<T> {
	var value:T;
	public function new(val:T) {
		this.value = val;
	}
	static function main() {
		var cl = Test;
		new Test(78.533567);
		Std.string(898687);
		triangular(20);
		reltest();
	}
	static function reltest() {
		return 8;
	}
	static function triangular(n:Int) {
		for(i in 1...n)
			n += i;
		return n;
	}
}
class SubTest<T> extends Test<Int> {
	public function new() {
		super(87);
	}
}