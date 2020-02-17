package cases;

class Issue7114 extends DisplayTestCase {
	/**
		import haxe.macro.Expr;
		import haxe.macro.Context;
		import haxe.macro.ExprTools;

		using haxe.macro.ExprTools;

		class Macro {
		public static macro function example_macro(e:Expr):Expr {
			trace('Block before macro:\n${e.toString()}');

			function injectCasts(ident:String, ct:ComplexType, expr:Expr):Expr {
				if (expr == null)
					return null;
				switch (expr.expr) {
					case EConst(CIdent(id)) if (id == ident):
						var rtn = macro((cast $i{ident}) : $ct);
						rtn.pos = expr.pos;
						return rtn;
					default:
				}

				return ExprTools.map(expr, function(e:Expr) return injectCasts(ident, ct, e));
			}

			var blk = injectCasts('macro_override_cast', (macro:String), e);

			var rtn = macro {
				var macro_injected:String = "injected";
				var macro_override_var:String = cast macro_override_var;
				$blk;
			};

			trace('Block after macro:\n${rtn.toString()}');
			return rtn;
		}
		}

		class Test {
		static function main() {
			var macro_override_var:SomeDynamic = "barbar";
			var macro_override_cast:SomeDynamic = "foofoo";

			Macro.example_macro({
				macro_injected.{-1-}
				macro_override_var.{-2-}
				macro_override_cast.{-3-}
			});
		}
		}

		abstract SomeDynamic(Dynamic) from Dynamic {
		public function oh_no__wrong_completions() {}
		}
	**/
	function test() {
		eq(true, hasField(fields(pos(1)), "length", "Int"));
		eq(true, hasField(fields(pos(2)), "length", "Int"));
		eq(true, hasField(fields(pos(3)), "length", "Int"));
	}
}
