package cases.display;

class Abstract extends DisplayTestCase {
	/**
		abstract A(Int) {
			public function new({-3-}i{-4-}) {
				this = {-1-}i;
				trace("f{-2-}oo");
			}
		}
	**/
	function test1(_) {
		Assert.same(range(3, 4), position(1));
		eq("String", type(2));
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
	function test2(_) {
		var top1 = toplevel(1);
		eq(true, hasToplevel(top1, "member", "instanceField", "() -> Void"));
		eq(true, hasToplevel(top1, "static", "staticField", "() -> Void"));

		var top2 = toplevel(2);
		eq(false, hasToplevel(top2, "member", "instanceField", "() -> Void"));
		eq(true, hasToplevel(top2, "static", "staticField", "() -> Void"));

		var f = fields(3);
		eq(false, hasField(f, "instanceField", "() -> Void"));
		eq(true, hasField(f, "staticField", "() -> Void"));
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
			public function instanceField2():Void {
				ab{-3-}stract;
			}
		}
		abstract AbGeneric<T>(T) {
			public function new(a:T) this = a;
			public function foo() {
				return ab{-4-}stract.bar();
			}
			public function bar() {
				return th{-5-}is;
			}
		}
	**/
	function test3(_) {
		final f1 = toplevel(1);
		eq(true, hasToplevel(f1, "literal", "abstract"));
		final f2 = toplevel(2);
		eq(false, hasToplevel(f2, "literal", "abstract"));
		eq("MyAbstract", type(3));
		eq("AbGeneric<AbGeneric.T>", type(4));
		eq("AbGeneric.T", type(5));
	}
}
