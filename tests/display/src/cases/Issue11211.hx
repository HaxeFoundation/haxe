package cases;

class Issue11211 extends DisplayTestCase {
	/**
		import haxe.macro.Context;
		import haxe.macro.Expr;
		using haxe.macro.Tools;

		class SafeAst {
			macro public static function build():Array<Field> {
				final fields = Context.getBuildFields();
				for (field in fields) {
					var expr:Null<Expr> = switch (field.kind) {
						case FVar(_, e): e;
						case FProp(_, _, _, e): e;
						case FFun(fn): fn.expr;
					}
					if (expr != null) {
						expr.expr = transform(expr).expr;
					}
				}
				return fields;
			}

			static function transform(expr:Expr):Expr {
				return expr.map(transform);
			}
		}

		#if !macro
		@:build(cases.Issue11211.SafeAst.build())
		class Main {
			static function main() {
				var errRa{-1-}nge = 0;
				{-4-}final pre{-2-}vId = trace("arg");{-5-}
				if (errRange != null) {
					final has{-3-}Comma = false;
				}
			}
		}
		#end
	**/
	function test() {
		eq("Int", type(pos(1)));
		eq("Void", type(pos(2)));
		eq("Bool", type(pos(3)));
		arrayEq([
			{
				kind: DKCompilerError,
				severity: Error,
				code: null,
				range: diagnosticsRange(pos(4), pos(5)),
				relatedInformation: [],
				args: "Variables of type Void are not allowed"
			},
		], diagnostics());
	}
}
