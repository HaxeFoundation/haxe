typedef MyType = {name:String}

class Foo {
	public static function f() {}
}

class Bar {
	public static function main() {
		std.MyType.Foo.f();
	}
}
