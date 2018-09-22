package cases;

import SignatureHelp;

class ArrayAccessSignature extends DisplayTestCase {
	/**
	class Some {
		function main() {
			[][{-1-}
		}
	}
	**/
	function testArray1() {
		sigEq(0, [["index:Int"]], signature(pos(1)));
	}

	/**
	class Some {
		function main() {
			[][{-1-}]
		}
	}
	**/
	function testArray2() {
		sigEq(0, [["index:Int"]], signature(pos(1)));
	}

	/**
	class Some {
		function main() {
			[][1{-1-}
		}
	}
	**/
	function testArray3() {
		sigEq(0, [["index:Int"]], signature(pos(1)));
	}

	/**
	class Some {
		function main() {
			[][1{-1-}]
		}
	}
	**/
	function testArray4() {
		sigEq(0, [["index:Int"]], signature(pos(1)));
	}

	/**
	class Some {
		function main() {
			[][1{-1-}2
		}
	}
	**/
	function testArray5() {
		sigEq(0, [["index:Int"]], signature(pos(1)));
	}

	/**
	class Some {
		function main() {
			[][1{-1-}2]
		}
	}
	**/
	function testArray6() {
		sigEq(0, [["index:Int"]], signature(pos(1)));
	}

	/**
	class Some {
		function main() {
			[1 => 2][{-1-}
		}
	}
	**/
	function testMap1() {
		// because screw consistency
		sigEq(0, [["key:Int"], ["k:Int"]], signature(pos(1)));
	}

	/**
	class Some {
		function main() {
			call([1 => 2][{-1-}
		}

		static function call(i1:Int, i2:Int) { }
	}
	**/
	function testInCall1() {
		sigEq(0, [["key:Int"], ["k:Int"]], signature(pos(1)));
	}
}