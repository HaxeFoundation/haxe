package cases.display;

class ArrayAccessSignature extends DisplayTestCase {
	/**
		class Some {
			function main() {
				[][{-1-}
			}
		}
	**/
	function testArray1(_) {
		sigEq(0, [["index:Int"]], signature(1));
	}

	/**
		class Some {
			function main() {
				[][{-1-}]
			}
		}
	**/
	function testArray2(_) {
		sigEq(0, [["index:Int"]], signature(1));
	}

	/**
		class Some {
			function main() {
				[][1{-1-}
			}
		}
	**/
	function testArray3(_) {
		sigEq(0, [["index:Int"]], signature(1));
	}

	/**
		class Some {
			function main() {
				[][1{-1-}]
			}
		}
	**/
	function testArray4(_) {
		sigEq(0, [["index:Int"]], signature(1));
	}

	/**
		class Some {
			function main() {
				[][1{-1-}2
			}
		}
	**/
	function testArray5(_) {
		sigEq(0, [["index:Int"]], signature(1));
	}

	/**
		class Some {
			function main() {
				[][1{-1-}2]
			}
		}
	**/
	function testArray6(_) {
		sigEq(0, [["index:Int"]], signature(1));
	}

	/**
		class Some {
			function main() {
				[1 => 2][{-1-}
			}
		}
	**/
	function testMap1(_) {
		// because screw consistency
		sigEq(0, [["key:Int"], ["k:Int", "v:Int"]], signature(1));
	}

	/**
		class Some {
			function main() {
				call([1 => 2][{-1-}
			}

			static function call(i1:Int, i2:Int) { }
		}
	**/
	function testInCall1(_) {
		sigEq(0, [["key:Int"], ["k:Int", "v:Int"]], signature(1));
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
	function testVoidReturn(_) {
		sigEq(0, [["k:Int"]], signature(1));
	}
}
