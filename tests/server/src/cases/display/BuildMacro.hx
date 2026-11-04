package cases.display;

class BuildMacro extends DisplayTestCase {
	/**
		class MyMacro {
			macro static public function build():Array<haxe.macro.Expr.Field> {
				var fields = haxe.macro.Context.getBuildFields();
				return fields;
			}
		}

		typedef {-7-}MyString{-8-} = String;

		#if !macro
		@:build(Main.MyMacro.build())
		#end
		class Main {
			function te{-1-}st({-5-}na{-2-}me{-6-}:MySt{-3-}ring):MyStr{-4-}ing {
				return {-9-}nam{-10-}e{-11-};
			}

			static function main() { }
		}
	**/
	function test1(_) {
		eq("MyString", type(2));
		eq("MyString", type(3));
		eq("MyString", type(4));
		Assert.same(range(7, 8), position(3));
		Assert.same(range(7, 8), position(4));
		Assert.same(range(5, 6), position(2));
		arrayEq([range(9, 11)], usage(10));
	}
}
