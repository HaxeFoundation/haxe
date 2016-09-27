package unit.issues.misc;

import haxe.macro.Context;
import haxe.macro.Expr;

class Issue3460Macro {
	macro static public function getOverloadString(e:Expr) {
		var c = switch (Context.typeof(e)) {
			case TInst(c, _): c.get();
			case _: throw "Something went wrong";
		}
		var cf = c.fields.get()[0];
		var s = cf.overloads.get().map(function (cf) return haxe.macro.TypeTools.toString(cf.type)).join(", ");
		return macro $v{s};
	}
}