import haxe.Constraints.NotVoid;

typedef FakeVoid = Void;

class Fail {
	public static function main() {
		test(void);
		test(fakeVoid);
	}

	static function void():Void {}
	static function fakeVoid():FakeVoid {}
	static function test<T:NotVoid>(f:()->T):T return f();
}
