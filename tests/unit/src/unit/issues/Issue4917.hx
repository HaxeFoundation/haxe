package unit.issues;

class Issue4917 extends Test {
	static function throwing(expected:Dynamic):Bool {
		switch(Type.typeof(expected)) {
			case TClass(c):
				Std.string(expected);
				return true;
			case TEnum(e) :
				Std.string(expected);
				return true;
			case _ :
				throw "Unable to compare two unknown types";
		}
		throw "Unable to compare values: " + expected;
	}

	function test() {
		t(throwing(new C()));
	}
}

private class C {
	public function new() { }
}