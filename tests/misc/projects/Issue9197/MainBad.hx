class Bar {
	public static var someVar : String = "Yay";
}

enum Foo {
	Bar;
}

class MainBad {
	static function main() {
		Bar.someVar = "test";
	}
}