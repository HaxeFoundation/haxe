function main() {
	Foo.test();
}

class Extensions {
	public static function test<T>(c:Class<T>) trace("ok");
}

@:build(Macro.build())
class Foo {}
