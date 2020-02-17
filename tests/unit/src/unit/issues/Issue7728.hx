package unit.issues;

private enum E {
	A(s:S);
}

private abstract S(String) {
	public var value(get, never):String;
	function get_value():String return this;
}

class Issue7728 extends unit.Test {
	function test() {
		noAssert();
	}

	static function doTest(e:E):Void {
		switch(e) {
			case A(value):
		}
	}
}