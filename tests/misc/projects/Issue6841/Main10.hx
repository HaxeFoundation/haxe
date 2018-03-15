class A {
	public function new() {}
	function toString() return 'a';
}

class B extends A {
	override function toString() return 'b';
}

class Main10 {
	static function main() {
		var a : Array<A> = [new B(), new A(), new B()];
		Sys.print(a.toString());
	}
}
