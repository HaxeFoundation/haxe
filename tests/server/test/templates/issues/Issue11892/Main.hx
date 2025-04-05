class Main {
	static function main():Void {
		var pure {-1-}= 42;
	}

	var value = {
		var y {-2-}= 42;
		0;
	};
}

class Foo {
	@:pure function foo() {}
}

class PureExpect extends Foo {
	override final function foo() {
		var y {-3-}= 42;
	}

	function baz() trace(DirectlyUsedEnum);
}

enum DirectlyUsedEnum {
	Foobar(a:Int {-4-}, b:Int);
}
