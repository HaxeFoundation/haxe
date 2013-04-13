import rust.*;
class Test<T> {
	var value:T;
	public function new(val:T) {
		this.value = val;
	}
	static function main() {
		new Test(78.533567);
		Std.string(898687);
	}
}
class SubTest<T> extends Test<Int> {
	public function new() {
		super(87);
	}
}