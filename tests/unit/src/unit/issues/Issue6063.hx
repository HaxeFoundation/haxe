package unit.issues;

import haxe.macro.Context;
import haxe.macro.Expr;
import unit.HelperMacros.*;

class Issue6063 extends Test {
	macro static function convert(e:Expr) {
		var t = Context.typeof(e);
		var ct = Context.toComplexType(t);
		return macro @:pos(e.pos) (null : $ct);
	}

	function testFinal() {
		#if !macro
		var x:{final s:String;} = null;
		var y = convert(x);
		x = y;
		t(typeError(y.s = "foo"));
		#end
	}

	function testAccess() {
		var o:{var x(default, null):Int;} = null;
		var o2 = convert(o);
		o = o2;
		t(typeError(o2.x = 12));
	}

	function testFunction() {
		var f:(?a:String) -> Void = null;
		var e = convert(f);
		eq("(?a : String) -> Void", typeString(e));
	}
}
