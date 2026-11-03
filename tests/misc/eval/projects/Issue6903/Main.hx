class Main {
	static function main() {}
}

abstract Test(Int) {
	@:op(A = B) static function opAssign(a:Test, b:Test):Test {
		return a;
	}
}