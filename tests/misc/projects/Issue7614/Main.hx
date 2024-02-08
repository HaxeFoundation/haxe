class Main {
	static function main() { }
}

@:forward
abstract Test(Test2) {
	public inline function new() {
		this.prop;
	}
}

@:forward
abstract Test2(Test) {}