package unit.issues.misc;

import haxe.macro.Expr;

class Issue2003Macro {
	public function new() {}

	public function callMe( cb:()->Void ) {
		cb();
		return this;
	}

	public static function create() {
		return new Issue2003Macro();
	}

	macro public function testMacro( ethis : Expr ) : Expr {
		var vname = "_____ref";
		var vdecl = macro var $vname = $ethis;
		var v = { expr : EConst(CIdent(vname)), pos : haxe.macro.Context.currentPos() };
		var eret = macro (function() { $vdecl;  return $v; })();
		return eret;
	}
}