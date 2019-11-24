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
		sigEq(0, [["key:Int"], ["k:Int", "v:Int"]], signature(pos(1)));
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
		sigEq(0, [["key:Int"], ["k:Int", "v:Int"]], signature(pos(1)));
	}

	/**
		abstract MyArray<T>(Array<T>) {
			public function new() this = [];

			@:op([])
			function arrayRead(k:Int):T return cast this[k];

			@:op([])
			function arrayReadWrite(k:Int, v:T) this[k] = v;
		}
		class Some {
			function main() {
				var m = new MyArray();
				var k = m[{-1-}
			}
		}
	**/
	function testVoidReturn() {
		sigEq(0, [["k:Int"]], signature(pos(1)));
	}
}
