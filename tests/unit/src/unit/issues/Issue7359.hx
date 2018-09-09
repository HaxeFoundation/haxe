package unit.issues;

import haxe.macro.Context;
import haxe.macro.Expr;

class Issue7359 extends unit.Test {
	function test() {
		#if !macro
		eq("no", m(11));
		eq("yes", m(1));
		#end
	}

	static macro function m(e) {
		var stored = haxe.macro.Context.storeTypedExpr(haxe.macro.Context.typeExpr(e));
		var got = haxe.macro.Context.getTypedExpr(haxe.macro.Context.typeExpr(e));

		// When using storedTypedExpr switch is invalid
		var c = {expr:macro "yes", guard:null, values:[stored]};

		var s = {expr:ESwitch(macro 1, [c], macro "no"), pos:Context.currentPos()};
		return s;
	}
}