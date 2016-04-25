package cases;

class Basic extends DisplayTestCase {
	/**
	class Some {
		function main() {
			var a = 5;
			a{-1-}
		}
	}
	**/
	function testType1() {
		eq("Int", type(pos(1)));
	}

	/**
	class Some {
		function main() {
			var {-1-}variable{-2-} = 5;
			variable{-3-};
		}
	}
	**/
	function testPosition1() {
		eq(range(1, 2), position(pos(3)));
	}

	/**
	class Some {
		function main() {
			var variable{-1-} = 5;
			{-2-}variable{-3-};
		}
	}
	**/
	function testUsage1() {
		eq(range(2, 3), usage(pos(1))[0]);
	}

	/**
	class Some {
		function main() {
			var variable{-1-} = 5;
			{-2-}variable{-3-};
			{-4-}variable{-5-};
		}
	}
	**/
	function testUsage2() {
		arrayEq([range(2, 3), range(4, 5)], usage(pos(1)));
	}
}
