package cases;

class InMacro extends DisplayTestCase {
	/**

		import haxe.macro.Context;
		import haxe.macro.Expr;

		class Main {

			#if macro

			static function buildTypeDef( {-4-}t{-5-} : haxe.macro.Type ) : Expr {
				Context.error("Unsupp{-1-}orted type "+Std.string({-2-}t{-3-}),Context.currentPos());
				return null;
			}

			#else

			static function buildTypeDef( {-14-}t{-15-} : haxe.macro.Type) : Expr {
				trace("not{-11-} a macro" + {-12-}t{-13-});
				return null;
			}

			#end
		}
	**/
	function testMacro1() {
		eq("String", type(pos(1)));
		arrayEq([range(2, 3)], usage(pos(4)));
		eq(range(4, 5), position(pos(2)));

		eq("String", type(pos(11)));
		arrayEq([range(12, 13)], usage(pos(14)));
		eq(range(14, 15), position(pos(12)));
	}
}
