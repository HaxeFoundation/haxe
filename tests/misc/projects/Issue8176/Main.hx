function main() {
	Test.func();
}

@:keep
@:native("Test")
class TestStub {
	static function func() {}
}

class Test {
	public static function func() {
		trace(Unused);
	}
}

class Unused {
	static function __init__() throw new haxe.Exception("this should be eliminated");
}
