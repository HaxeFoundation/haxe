package unit.issues;

private class A<S, T> {
	public var v:Int;
	public function new() { }
	public function f() { }
}

private typedef T = {
	function f():Void;
}

class Issue4126 extends Test {
	function test() {
		var a = new A<String, Int>();
		eq("A<String, Int>", getFieldAccessString(a.v));
		eq("A<String, Int>", getFieldAccessString(a.f));
		eq("String", getFieldAccessString("foo".length));
		#if !python
		eq("String", getFieldAccessString("foo".charAt));
		#end
		var t:T = null;
		eq("null", getFieldAccessString(t.f));
	}

	macro static function getFieldAccessString(e) {
		var et = haxe.macro.Context.typeExpr(e);
		inline function shortPrint(c, tl) {
			var a = haxe.macro.TypeTools.toString(TInst(c, tl)).split(".");
			return a.pop();
		}
		var s = switch (et.expr) {
			case TField(_, FClosure(co, _)):
				if (co == null) {
					"null";
				} else {
					shortPrint(co.c, co.params);
				}
			case TField(_, FInstance(c, tl, _)):
				shortPrint(c, tl);
			case _:
				"invalid: " + haxe.macro.TypedExprTools.toString(et);
		}
		return macro $v{s};
	}
}