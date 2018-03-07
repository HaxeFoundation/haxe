class A {
	public function new() {}
	function toString() return 'a';
}

class Main6 {
	static function main() {
		Sys.print([1 => new A(), 2 => new A()].toString());
	}
}
