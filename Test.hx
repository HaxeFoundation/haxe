import rust.*;
class Test<T> {
	var value:T;
	public function new(val:T) {
		this.value = val;
	}
	static function main() {
		new Test(78.533567);
		Sys.println(Std.string(898687));
		Sys.println("Hello, world!");
		Sys.println(Std.string(Math.floor(897*98797.8799)));
	}
}
class SubTest<T> extends Test<Int> {
	public function new() {
		super(87);
	}
}