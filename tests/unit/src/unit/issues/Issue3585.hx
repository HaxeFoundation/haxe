package unit.issues;
import unit.Test;

class Issue3585 extends Test {
	#if static // this is only relevant on static platforms
	function test() {
		var v = inlineUnwrap();
		eq(getType(v), "Int");
	}

	inline function inlineUnwrap():Int {
		var tmp:Null<Int> = 1;
		return tmp;
	}
	#end

	static macro function getType(v:haxe.macro.Expr) {
		var t = haxe.macro.Context.typeof(v);
		return macro $v{haxe.macro.TypeTools.toString(t)};
	}
}
