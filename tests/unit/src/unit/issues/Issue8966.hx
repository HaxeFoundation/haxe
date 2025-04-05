package unit.issues;

#if macro
import haxe.macro.Expr;
import haxe.macro.ExprTools;
import haxe.macro.MacroStringTools;
#end

class Issue8966 extends Test {
	function test() {
		var i = 10;
		eq("foo10", m('foo$i'));
	}

	static macro function m(e) {
		e.pos = (macro here).pos;
		return macro {
			t($v{MacroStringTools.isFormatExpr(e)});
			$e;
		};
	}
}
