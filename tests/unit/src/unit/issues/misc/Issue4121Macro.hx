package unit.issues.misc;

import haxe.macro.Expr;
using haxe.macro.Tools;

class Issue4121Macro {
	macro static public function wrap(e:Expr) {
		function addParens(e:Expr) {
			return macro (${e.map(addParens)});
		}
		var e = addParens(e);
		return macro $v{e.toString()};
	}
}