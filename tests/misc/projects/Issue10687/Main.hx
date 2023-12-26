class Main {
	var foo = new Foo();
	var fooExtern = new Foo();
	var fooAb = new FooExtern();
	var vector = new haxe.ds.Vector(1);
	var vector2 = new haxe.ds.Vector(1, 0);
	static function main() {}
}

class Foo {
	public var length = Std.random(1);
	public inline function new() {}
}

class FooExtern {
	public var length = Std.random(1);
	extern public inline function new() {}
}

abstract FooAb({length:Int}) {
	extern public inline function new() {
		this = {length: Std.random(1)};
	}
}
