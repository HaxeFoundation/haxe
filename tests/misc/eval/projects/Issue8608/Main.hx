class Main {
	static function main() {}
}

abstract Test(Test2) {
	public inline function new() {
		this = null;
	}
}

abstract Test2(Test) {}