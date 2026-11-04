import haxe.macro.Context;
import haxe.macro.Expr;

using haxe.macro.Tools;

#if !macro
@:build(Main.build())
#end
class Main {
	#if macro
	static function build():Array<Field> {
		var t = Context.typeof(macro M.x);
		var field = (macro class X {
			static public var type = $v{t.toString()};
		}).fields[0];
		return [field];
	}
	#end
}

function main() {
	#if !macro
	trace(Main.type);
	#end
}
