interface IA {
	public var a:String;
}

abstract class A implements IA {
	public function new() {}
}

class B extends A {
	public function new() {
		super();
	}
}

function main() {}
