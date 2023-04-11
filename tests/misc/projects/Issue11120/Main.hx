import Main.foo;

@:build(B.build())
class Main {
	static function main() {
		Sys.println("all good");
	}

	public static function foo() {
		return "foo";
	}
}
