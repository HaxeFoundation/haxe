package cases;

class Resuming extends DisplayTestCase {
	/**
		class {-1-}C1{-2-} { }
		typedef TUnfinished = syntax gone wrong }]){[{.,-+*%/^&&||
		class {-3-}C2{-4-} { }
	**/
	function testTypeResume1() {
		eq(range(1, 2), position(pos(1)));
		eq(range(3, 4), position(pos(3)));
	}

	/**
		typedef TUnfinished = Arr{-1-}ay<{signatures: Ar{-2-}ray<St{-3-}ring
	**/
	function testAutoClose1() {
		eq("Array<Array.T>", type(pos(1)));
		eq("Array<Array.T>", type(pos(2)));
		eq("String", type(pos(3)));
	}
}
