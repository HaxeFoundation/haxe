class A {
	public function new() {}
	function toString() return 'a';
}

class Main1 {
	static function main() {
		Sys.print([new A(), new A(), new A()].join(''));
	}
}