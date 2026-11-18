import haxe.macro.Context;
import haxe.macro.Expr;

class Gen {
	public static function init() {
		Context.onAfterInitMacros(() -> {
			var p = Context.makePosition({file: "Gen.hx", min: 0, max: 0});
			for (name in ["GenA", "GenB"]) {
				var e:Expr = {expr: EReturn({expr: EConst(CInt("1")), pos: p}), pos: p};
				Context.defineType({
					pack: [],
					name: name,
					pos: p,
					kind: TDClass(),
					meta: [{name: ":keep", pos: p}],
					fields: [{
						name: "foo",
						pos: p,
						access: [APublic, AStatic],
						kind: FFun({args: [], ret: macro :Int, expr: e})
					}]
				});
			}
		});
	}
}
