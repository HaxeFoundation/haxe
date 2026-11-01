package cases.display;

class Override extends DisplayTestCase {
	/**
		class Super {
			public function test1() return 12;
			public function test2() return 12;
		}

		class Main extends Super {
			override function test1() { return 12; }
			override {-1-}function x() { }
		}
	**/
	function test1(_) {
		var f = fields(1);
		eq(true, hasField(f, "test2", "() -> Int"));
		eq(false, hasField(f, "test1", "() -> Int"));
	}

	/**
		class Super {
			public function test1() return 12;
			public function test2() return 12;
		}

		class Main extends Super {
			override {-1-}function x() { }
			override function test1() { return 12; }
		}
	**/
	function test2(_) {
		var f = fields(1);
		eq(true, hasField(f, "test2", "() -> Int"));
		eq(false, hasField(f, "test1", "() -> Int"));
	}

	/**
		class SuperSuper {
			public function test1() return 12;
			public function test2() return 12;
		}

		class Super extends SuperSuper { }

		class Main extends Super {
			override function test1() { return 12; }
			override {-1-}function x() { }
		}
	**/
	function test3(_) {
		var f = fields(1);
		eq(true, hasField(f, "test2", "() -> Int"));
		eq(false, hasField(f, "test1", "() -> Int"));
	}
}
