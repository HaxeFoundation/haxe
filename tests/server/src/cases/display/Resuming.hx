package cases.display;

class Resuming extends DisplayTestCase {
	/**
		class {-1-}C1{-2-} { }
		typedef TUnfinished = syntax gone wrong }]){[{.,-+*%/^&&||
		class {-3-}C2{-4-} { }
	**/
	function testTypeResume1(_) {
		Assert.same(range(1, 2), position(1));
		Assert.same(range(3, 4), position(3));
	}

	/**
		typedef TUnfinished = Arr{-1-}ay<{signatures: Ar{-2-}ray<St{-3-}ring
	**/
	function testAutoClose1(_) {
		eq("Array<Array.T>", type(1));
		eq("Array<Array.T>", type(2));
		eq("String", type(3));
	}
}
