package unit.issues;

import unit.Test;
#if !macro
import unit.issues.misc.Issue11712Macro;

@:genericBuild(unit.issues.misc.Issue11712Macro.build())
private class GBuild {}
#end

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

		// a macro can forward its (call-site) pos to a regular helper run in macro context
		eq("test", forwarded());

		// a @:genericBuild macro's ?pos is the use-site position
		unit.HelperMacros.typedAs((null : GBuild), (null : Issue11712Result<"test">));
	}
	#end

	// a regular function, also callable from the macro above at compile time
	static function helper(?pos:haxe.PosInfos):String {
		return pos.methodName;
	}

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

	macro static function forwarded(?pos:haxe.PosInfos) {
		// forward the auto-filled call-site pos to a regular function (runs in macro context)
		return macro $v{helper(pos)};
	}
}
