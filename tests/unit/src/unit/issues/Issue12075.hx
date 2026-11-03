package unit.issues;

@:keep
private class M {
	var __exceptionMessage:String;

	public function new(value:Any) {
		__exceptionMessage = value;
	}

	public function toString():String {
		return __exceptionMessage;
	}
}

class Issue12075 extends Test {
	function testNullException() {
		var str = Std.string(new haxe.Exception(null));
		noAssert();
	}

	function testNullToString() {
		var str = Std.string(new M(null));
		noAssert();
	}
}
