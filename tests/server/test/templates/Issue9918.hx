#if macro
import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
using haxe.macro.Tools;
#end

enum abstract Days(Int) to Int {
	var Sun = 0;
	var Mon;
	var Tue;
}
class Issue9918 {
	static function main() {
		Macros.weirdECast(Days);
	}
}
class Macros {
	macro public static function weirdECast( typePath : Expr ) {
		var type = Context.getType(typePath.toString());
		if(ecastCount(type) == 1) {
			trace('correct ECast count');
		}
		Context.error('error', (macro {}).pos);
		return macro {};
	}
#if macro
	static function ecastCount( type : Type ) {
		switch (type) {
		case TAbstract(_.get() => ab, _) if (ab.meta.has(":enum")):
			for (field in ab.impl.get().statics.get()) {
				var first = field.meta.get()[0];
				return strip(first.params[0], 0);
			}
		default:
		}
		return -1;
	}
	static function strip(e, acc) {
		return switch(e.expr) {
		case ECast(e, _):
			strip(e, acc + 1);
		case _:
			acc;
		}
	}
#end
}