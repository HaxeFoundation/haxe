package unit.issues.misc;
import haxe.macro.Expr;

class GenericBuildResult<Const> { }

class Issue3089Macro {
	macro static public function build():ComplexType {
		var s = switch (haxe.macro.Context.getLocalType()) {
			case TInst(_, self): self.map(function(t) return haxe.macro.TypeTools.toString(t)).join("_");
			default: throw "Something went wrong.";
		}
		var ct = TPath({name: "Issue3089Macro", pack: ["unit", "issues", "misc"], sub: "GenericBuildResult", params: [TPExpr(macro $v{s})]});
		return macro : $ct;
	}
}