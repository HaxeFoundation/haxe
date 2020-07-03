package cases;

class Abstract extends DisplayTestCase {
	/**
		abstract A(Int) {
			public function new({-3-}i{-4-}) {
				this = {-1-}i;
				trace("f{-2-}oo");
			}
		}
	**/
	function test() {
		eq(range(3, 4), position(pos(1)));
		eq("String", type(pos(2)));
	}

	/**
		abstract MyAbstract(String) {
			public function new() this = "foo";

			public function instanceField():Void {
				{-1-}
			}
			static public function staticField():Void {
				{-2-}
			}
		}
		class MyClass {
			static function main() {
				MyAbstract.{-3-}
			}
		}
	**/
	function test2() {
		var top1 = toplevel(pos(1));
		eq(true, hasToplevel(top1, "member", "instanceField", "() -> Void"));
		eq(true, hasToplevel(top1, "static", "staticField", "() -> Void"));

		var top2 = toplevel(pos(2));
		eq(false, hasToplevel(top2, "member", "instanceField", "() -> Void"));
		eq(true, hasToplevel(top2, "static", "staticField", "() -> Void"));

		var fields = fields(pos(3));
		eq(false, hasField(fields, "instanceField", "() -> Void"));
		eq(true, hasField(fields, "staticField", "() -> Void"));
	}
}
