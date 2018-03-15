class A {
	public function new() {}
	function toString() return 'a';
}

class B extends A {}

class Main7 {
	static function main() {
		Sys.print([new B(), new B(), new B()].toString());
	}
}
