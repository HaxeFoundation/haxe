enum Foo {
	Bar;
}

class Bar {
	public static var someVar : String = "Yay";
}

class MainGood {
	static function main() {
		Bar.someVar = "test";
	}
}