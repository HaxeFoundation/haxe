package cases;

class Issue7137 extends DisplayTestCase {
	/**
		abstract Foo1(Int) from {-1-} {}
		abstract Foo2(Int) to {-2-} {}

		class Main {
			static function main() {}
		}
	**/
	function test1() {
		eq(true, hasToplevel(toplevel(pos(1)), "type", "Float"));
		eq(true, hasToplevel(toplevel(pos(2)), "type", "Float"));
	}

	/**
		abstract Foo1(Int) from {-1-}
	**/
	function test2() {
		eq(true, hasToplevel(toplevel(pos(1)), "type", "Float"));
	}

	/**
		abstract Foo1(Int) from {-1-} {
			public function new() { }
		}
	**/
	function test3() {
		eq(true, hasToplevel(toplevel(pos(1)), "type", "Float"));
	}
}
