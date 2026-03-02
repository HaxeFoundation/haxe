package cases.display;

class Basic extends DisplayTestCase {
	/**
		class Some {
			function main() {
				var a = 5;
				a{-1-}
			}
		}
	**/
	function testType1(_) {
		eq("Int", type(1));
	}

	/**
		class Some {
			function main() {
				var {-1-}variable{-2-} = 5;
				{-3-}variabl{-4-}e{-5-};
			}
		}
	**/
	function testPosition1(_) {
		Assert.same(range(1, 2), position(3));
		Assert.same(range(1, 2), position(4));
		Assert.same(range(1, 2), position(5));
	}

	/**
		class Some {
			function main() {
				var variabl{-1-}e = 5;
				{-2-}variable{-3-};
			}
		}
	**/
	function testUsage1(_) {
		Assert.same(range(2, 3), usage(1)[0]);
	}

	/**
		class Some {
			function main() {
				var variabl{-1-}e = 5;
				{-2-}variable{-3-};
				{-4-}variable{-5-};
			}
		}
	**/
	function testUsage2(_) {
		arrayEq([range(2, 3), range(4, 5)], usage(1));
	}

	/**
		class Some {
			function main() {
				{-1-}te{-2-}st{-3-}();
			}

			static function te{-4-}st() { }
		}
	**/
	function testUsage3(_) {
		arrayEq([range(1, 3)], usage(2));
		arrayEq([range(1, 3)], usage(4));
	}

	/**
		class Main {
			static function main() {
				function test(na{-1-}me:St{-2-}ring) { }
			}
		}
	**/
	function testLocalFunction(_) {
		eq("String", type(1));
		eq("String", type(2));
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
	function testFieldDisplay(_) {
		eq("String", type(1));
		eq("String", type(2));
		eq("String", type(3));
		eq("String", type(4));
		eq("String", type(5));
		eq("String", type(6));
	}

	/**
		class Main {
			static function main() {
				var a:Ar{-1-}ray;
			}
		}
	**/
	function testMissingParams(_) {
		eq("Array<Array.T>", type(1));
	}

	/**
		class Some {
			function new(someName:Int) {}
			function main() {
				Some.new{-1-}
			}
		}
	**/
	function testCtorClosureType(_) {
		eq("(someName : Int) -> Some", type(1));
	}
}
