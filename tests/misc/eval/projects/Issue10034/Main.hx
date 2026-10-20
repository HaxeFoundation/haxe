#if macro
class Foo {
	public static function build(f:() -> Void):Array<haxe.macro.Expr.Field>
		return [];

	public static function other():Void {}
}
#else
@:build(Foo.build(Foo.other))
class Main {}

class Foo {}
#end
