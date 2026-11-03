class Main {
	static function main() {
		(null:B).f();
	}
}

abstract class A {
	abstract public function f():Void;
}

class B extends A {
	function f() {}
}

class C extends A {
	override function f() {}
}

class E extends A {
	private function f() {}
}