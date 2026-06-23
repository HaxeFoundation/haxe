package unit.issues.misc;

import haxe.macro.Expr;

// carries the captured call-site method name as a const type parameter
class Issue11712Result<Const> {}

class Issue11712Macro {
	// a @:genericBuild macro may take a trailing ?pos:haxe.PosInfos, filled with the
	// use-site position (where the built type is referenced)
	macro static public function build(?pos:haxe.PosInfos):ComplexType {
		var ct = TPath({
			name: "Issue11712Macro",
			pack: ["unit", "issues", "misc"],
			sub: "Issue11712Result",
			params: [TPExpr(macro $v{pos.methodName})]
		});
		return macro : $ct;
	}
}
