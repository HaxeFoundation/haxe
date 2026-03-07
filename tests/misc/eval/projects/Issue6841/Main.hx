class A {
	public function new() {}

	function toString()
		return 'a';
}

class Main {
	static function main() {
		var foos: Array<A> = [new A(), new A(), new A()];
		Sys.print(foos.join(''));
	}
}