package cases;

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
	@:build(cases.BuildMacro.MyMacro.build())
	#end
	class Main {
		function te{-1-}st({-5-}na{-2-}me{-6-}:MySt{-3-}ring):MyStr{-4-}ing {
			return {-9-}nam{-10-}e{-11-};
		}

		static function main() { }
	}
	**/
	function test1() {
		eq("cases.MyString", type(pos(2)));
		eq("cases.MyString", type(pos(3)));
		eq("cases.MyString", type(pos(4)));
		eq(range(7, 8), position(pos(3)));
		eq(range(7, 8), position(pos(4)));
		eq(range(5, 6), position(pos(2)));
		arrayEq([range(9, 11)], usage(pos(10)));
	}
}