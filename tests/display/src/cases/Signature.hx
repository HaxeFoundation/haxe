package cases;

import SignatureHelp;

class Signature extends DisplayTestCase {
	/**
	class Some {
		function main() {
			test({-1-}"foo"{-2-},{-3-}12{-4-})
		}

		static function test(a:String, b:Int) { }
	}
	**/
	function testGoodAst() {
		sigEq(0, [["a:String", "b:Int"]], signature(pos(1)));
		sigEq(0, [["a:String", "b:Int"]], signature(pos(2)));
		sigEq(1, [["a:String", "b:Int"]], signature(pos(3)));
		sigEq(1, [["a:String", "b:Int"]], signature(pos(4)));
	}

	/**
	class Some {
		function main() {
			test({-1-}"foo"{-2-}
		}

		static function test(a:String, b:Int) { }
	}
	**/
	function testBadAst() {
		sigEq(0, [["a:String", "b:Int"]], signature(pos(1)));
		sigEq(0, [["a:String", "b:Int"]], signature(pos(2)));
	}

	/**
		class Some {
			static function someFunc(a:String, b:Float) { }

			static function main() {
				var a = "foo";
				var b = 12;
				someFunc(a, {v: [a, b, {-1-}
			}
		}
	**/
	function testUglyAst() {
		sigEq(1, [["a:String", "b:Float"]], signature(pos(1)));
	}

	/**
	class Some {
		function main() {
			test({-1-}
		}

		@:overload(function (a:String, b:Bool):Void { })
		static function test(a:String, b:Int) { }
	}
	**/
	function testOverloads1() {
		sigEq(0, [["a:String", "b:Bool"], ["a:String", "b:Int"]], signature(pos(1)));
	}

	/**
	class Some {
		function main() {
			test("foo",{-1-}
		}

		@:overload(function (a:String, b:Bool):Void { })
		static function test(a:String, b:Int) { }
	}
	**/
	function testOverloads2() {
		sigEq(1, [["a:String", "b:Bool"], ["a:String", "b:Int"]], signature(pos(1)));
	}

	/**
	class Some {
		function main() {
			test("foo", 12{-1-}
		}

		@:overload(function (a:String, b:Bool):Void { })
		static function test(a:String, b:Int) { }
	}
	**/
	function testOverloads3() {
		sigEq(1, [["a:String", "b:Int"]], signature(pos(1)));
	}

	/**
	class Some {
		function main() {
			test({-1-} {-2-}
			var x = 12;
		}

		static function test(a:String, b:Int) { }
	}
	**/
	function testInsert1() {
		sigEq(0, [["a:String", "b:Int"]], signature(pos(1)));
		sigEq(0, [["a:String", "b:Int"]], signature(pos(2)));
	}

	/**
	class Some {
		function main() {
			test("foo",{-1-} {-2-}
			var x = 12;
		}

		static function test(a:String, b:Int) { }
	}
	**/
	function testInsert2() {
		sigEq(1, [["a:String", "b:Int"]], signature(pos(1)));
		sigEq(1, [["a:String", "b:Int"]], signature(pos(2)));
	}

	/**
	class SomeBase {
		function new(a:Bool, b:Float) { }
	}

	class Some extends SomeBase {
		function new(a:String, b:Int) {
			super({-1-});
			super(true, 1{-2-});
		}

		static function main() {
			new Some({-3-}"fo{-4-}o"{-5-},{-6-} {-7-}12{-8-});
		}
	}
	**/
	function testConstructorGood() {
		sigEq(0, [["a:Bool", "b:Float"]], signature(pos(1)));
		sigEq(1, [["a:Bool", "b:Float"]], signature(pos(2)));
		sigEq(0, [["a:String", "b:Int"]], signature(pos(3)));
		sigEq(0, [["a:String", "b:Int"]], signature(pos(4)));
		sigEq(0, [["a:String", "b:Int"]], signature(pos(5)));
		sigEq(1, [["a:String", "b:Int"]], signature(pos(6)));
		sigEq(1, [["a:String", "b:Int"]], signature(pos(7)));
		sigEq(1, [["a:String", "b:Int"]], signature(pos(8)));
	}

	/**
	class Some {
		function new(a:String, b:Int) { }

		static function main() {
			new Some({-1-}
		}
	}
	**/
	function testConstructorBad() {
		sigEq(0, [["a:String", "b:Int"]], signature(pos(1)));
	}

	/**
	class Some {
		static function main() {
			test(test({-1-}
		}

		static function test(a:String, b:Int) { }
	}
	**/
	function testNested() {
		sigEq(0, [["a:String", "b:Int"]], signature(pos(1)));
	}

	function sigEq(arg:Int, params:Array<Array<String>>, sig:SignatureHelp, ?pos:haxe.PosInfos) {
		eq(arg, sig.activeParameter, pos);
		eq(params.length, sig.signatures.length, pos);
		for (i in 0...params.length) {
			var sigInf = sig.signatures[i];
			var args = params[i];
			eq(sigInf.parameters.length, args.length, pos);
			for (i in 0...args.length) {
				eq(sigInf.parameters[i].label, args[i], pos);
			}
		}
	}
}