package cases;

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
	function test1() {
		var fields = fields(pos(1));
		eq(true, hasField(fields, "test2", "() -> Int"));
		eq(false, hasField(fields, "test1", "() -> Int"));
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
	function test2() {
		var fields = fields(pos(1));
		eq(true, hasField(fields, "test2", "() -> Int"));
		eq(false, hasField(fields, "test1", "() -> Int"));
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
	function test3() {
		var fields = fields(pos(1));
		eq(true, hasField(fields, "test2", "() -> Int"));
		eq(false, hasField(fields, "test1", "() -> Int"));
	}
}
