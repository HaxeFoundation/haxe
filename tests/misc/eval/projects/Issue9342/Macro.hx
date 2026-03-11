import haxe.macro.Context;
import haxe.macro.Expr;

class Macro {
	static public macro function foo() {
		var pos = Context.currentPos();
		return macro @:pos(pos) new Foo();
	}
#if macro
	static function buildFoo() {
		Context.warning('check pos', Context.currentPos());
		return Context.typeof(macro [1]);
	}
#end
}