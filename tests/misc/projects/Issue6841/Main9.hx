class A {
	public function new() {}
	function toString() return 'a';
}

class B extends A {
	override function toString() return 'b';
}

class Main9 {
	static function main() {
		Sys.print([new B(), new B(), new B()].toString());
	}
}
