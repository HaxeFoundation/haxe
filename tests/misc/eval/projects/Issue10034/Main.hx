#if macro
class Foo {
	public static function build(f:() -> Void)
		return [];

	public static function other():Void {}
}
#else
@:build(Foo.build(Foo.other))
class Main {}

class Foo {}
#end
