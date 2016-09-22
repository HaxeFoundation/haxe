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

	/**
	function test(na{-1-}me:St{-2-}ring) { }
	**/
	@:funcCode function testLocalFunction() {
		eq("String", type(pos(1)));
		eq("String", type(pos(2)));
	}

	/**
	class Some {
		var x1 = "fo{-1-}";
		static var x2 = "fo{-2-}";
		static inline var x3 = "fo{-3-}";

		var x4(default, null) = "fo{-4-}";
		static var x5(default, null) = "fo{-5-}";

		static var x{-6-}6:String;
	}
	**/
	function testFieldDisplay() {
		eq("String", type(pos(1)));
		eq("String", type(pos(2)));
		eq("String", type(pos(3)));
		eq("String", type(pos(4)));
		eq("String", type(pos(5)));
		eq("String", type(pos(6)));
	}

	/**
	var a:Ar{-1-}ray;
	**/
	@:funcCode function testMissingParams() {
		eq("Array<Array.T>", type(pos(1)));
	}
}
