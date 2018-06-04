class Main {
	public static function main() {
		new Foo(0);
	}
}

abstract Foo(Int) {
	function new(value:Int) this = value;
}