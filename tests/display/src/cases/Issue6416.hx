package cases;

class Issue6416 extends DisplayTestCase {
	/**
	class Main {
		static function main() {}

		macro function foo(e:haxe.macro.Expr) {
			switch (e) {
				case macro $i{{-1-}f{-2-}oo{-3-}}:
					f{-4-}oo;
			}
		}
	}
	**/
	function test() {
		eq(range(1, 3), position(pos(2)));
		eq(range(1, 3), position(pos(4)));
		eq("String", type(pos(2)));
	}
}
