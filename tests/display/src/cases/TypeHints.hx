package cases;

class TypeHints extends DisplayTestCase {
	/**
		typedef T1 = {-12-}T{-4-}2{-13-};
		typedef T{-3-}2 = {-10-}C{-2-}1{-11-};
		class C{-1-}1 { }
	**/
	function testTypedef1() {
		arrayEq([range(12, 13)], usage(pos(3)));
		arrayEq([range(12, 13)], usage(pos(4)));
		arrayEq([range(10, 11)], usage(pos(1)));
		arrayEq([range(10, 11)], usage(pos(2)));
	}

	/**
		typedef T1 = {
			var f1:{-10-}C{-1-}1{-11-};
			var f2:{-12-}C1{-13-};
		}

		class C{-2-}1 { }
	**/
	function testStructure1() {
		arrayEq([range(10, 11), range(12, 13)], usage(pos(1)));
		arrayEq([range(10, 11), range(12, 13)], usage(pos(2)));
	}

	/**
		enum E1 {
			C1(c:{-10-}C{-1-}1{-11-});
			C2(c:Int, d:{-12-}C{-2-}1{-13-});
		}

		class C{-3-}1 { }
	**/
	function testEnum1() {
		arrayEq([range(10, 11), range(12, 13)], usage(pos(1)));
		arrayEq([range(10, 11), range(12, 13)], usage(pos(2)));
		arrayEq([range(10, 11), range(12, 13)], usage(pos(3)));
	}

	/**
		abstract A1({-10-}C{-1-}1{-11-}) from {-12-}C{-2-}1{-13-} to {-14-}C{-3-}1{-15-} { }

		class C{-4-}1 { }
	**/
	function testAbstract1() {
		arrayEq([range(10, 11), range(12, 13), range(14, 15)], usage(pos(1)));
		arrayEq([range(10, 11), range(12, 13), range(14, 15)], usage(pos(2)));
		arrayEq([range(10, 11), range(12, 13), range(14, 15)], usage(pos(3)));
		arrayEq([range(10, 11), range(12, 13), range(14, 15)], usage(pos(4)));
	}

	/**
		class C{-1-}1 {
			static function test(c:{-10-}C{-2-}1{-11-}):{-12-}C{-3-}1{-13-} { }
		}
	**/
	function testFunction1() {
		arrayEq([range(10, 11), range(12, 13)], usage(pos(1)));
		arrayEq([range(10, 11), range(12, 13)], usage(pos(2)));
		arrayEq([range(10, 11), range(12, 13)], usage(pos(3)));
	}

	/**
		class C{-1-}1 { }
		class C2 extends {-10-}C{-2-}1{-11-} { }
	**/
	function testClass1() {
		arrayEq([range(10, 11)], usage(pos(1)));
		arrayEq([range(10, 11)], usage(pos(2)));
	}

	/**
		interface I{-1-}1 { }
		class C2 implements {-10-}I{-2-}1{-11-} { }
	**/
	function testInterface1() {
		arrayEq([range(10, 11)], usage(pos(1)));
		arrayEq([range(10, 11)], usage(pos(2)));
	}
}
