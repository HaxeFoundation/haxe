package unit.issues;

import unit.Test;

class Issue11712 extends Test {
	#if !macro
	function test() {
		// ?pos on a macro function is filled with the *call site*, not the macro's own pos
		eq("test", whereMethod());
		eq("unit.issues.Issue11712", whereClass());

		// works alongside a regular Expr argument
		eq("test", afterExpr("ignored"));

		// works alongside a rest argument
		eq("2:test", afterRest(1, 2));
		eq("0:test", afterRest());
	}
	#end

	macro static function whereMethod(?pos:haxe.PosInfos) {
		return macro $v{pos.methodName};
	}

	macro static function whereClass(?pos:haxe.PosInfos) {
		return macro $v{pos.className};
	}

	macro static function afterExpr(e:haxe.macro.Expr, ?pos:haxe.PosInfos) {
		return macro $v{pos.methodName};
	}

	macro static function afterRest(...rest:haxe.macro.Expr, ?pos:haxe.PosInfos) {
		return macro $v{rest.length + ":" + pos.methodName};
	}
}
