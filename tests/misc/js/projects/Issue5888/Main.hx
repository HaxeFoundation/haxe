using Main;

class Main {
	static function main() {
		0.bar();   //Should not compile.
	}
}

abstract Foo(Int) {
	public function bar() {};
}