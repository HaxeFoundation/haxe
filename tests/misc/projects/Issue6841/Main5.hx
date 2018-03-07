class A {
	public function new() {}
	function toString() return 'a';
}

class Main5 {
	static function main() {
		var b = new StringBuf();
		b.add([new A(), new A(), new A()].toString());
		Sys.print(b.toString());
	}
}
