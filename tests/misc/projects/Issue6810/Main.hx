import haxe.Constraints.NotVoid;

class Main {
	public static function main() {
		test(function() return 42);
		test(function() return "test");
	}

	static function test<T:NotVoid>(f:Void->T):T return f();
}

