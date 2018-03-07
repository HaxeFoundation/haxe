class A {
	public function new() {}
	function toString() return Std.string('a');
}

class Main3 {
	static function main() {
		var foos : Array<A> = [new A(), new A(), new A()];
		Sys.print(foos.toString());
	}
}