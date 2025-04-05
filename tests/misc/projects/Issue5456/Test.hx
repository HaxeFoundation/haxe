package;

import test.C;

@:build(TestBuilder.build())
class Test extends B {
	public function new() {
		super();
	}
}

class B {
	public var c:Int = C.func();

	public function new() {}
}
