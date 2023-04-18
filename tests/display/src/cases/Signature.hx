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
				q({-1-})
			}

			@:overload(function():Void {})
			@:overload(function(foo:Int, bar:Int):Void {})
			static function q(foo:Int) {}
		}
	**/
	function testOverloads4() {
		sigEq(0, [[], ["foo:Int", "bar:Int"], ["foo:Int"]], signature(pos(1)));
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

	/**
		class Some {
			static function main() {
				test2(test({-1-}
			}

			static function test(a:String, b:Int) { }
			static function test2(a:Int, b:String) { }
		}
	**/
	function testNestedNotPointless() {
		sigEq(0, [["a:String", "b:Int"]], signature(pos(1)));
	}

	/**
		class Main {

			static function main() {
				lotsOfArgs({-1-}  "f{-2-}oo" {-3-} {-4-},{-5-}  12{-6-},{-7-} {-8-}   {-9-},{-10-} {-11-} [{-12-}]{-13-},{-14-}

		{-15-}
			}

			static function lotsOfArgs(a:String, b:Int, c:Bool, d:Array<Int>, e:Dynamic) { }
		}
	**/
	function testBroken() {
		var sig = [["a:String", "b:Int", "c:Bool", "d:Array<Int>", "e:Dynamic"]];
		sigEq(0, sig, signature(pos(1)));
		sigEq(0, sig, signature(pos(2)));
		sigEq(0, sig, signature(pos(3)));
		sigEq(0, sig, signature(pos(4)));
		sigEq(1, sig, signature(pos(5)));
		sigEq(1, sig, signature(pos(6)));
		sigEq(2, sig, signature(pos(7)));
		sigEq(2, sig, signature(pos(8)));
		sigEq(2, sig, signature(pos(9)));
		sigEq(3, sig, signature(pos(10)));
		sigEq(3, sig, signature(pos(11)));
		sigEq(3, sig, signature(pos(12)));
		sigEq(3, sig, signature(pos(13)));
		sigEq(4, sig, signature(pos(14)));
		sigEq(4, sig, signature(pos(15)));
	}

	/**
		class NotMain {
			overload public function new(s:String) {}
			overload function new(i:Int) {}
		}

		class Main {
			static function main() {
				new NotMain({-1-}
			}
		}
	**/
	function testCtorVisibility() {
		sigEq(0, [["s:String"]], signature(pos(1)));
	}

	/**
		class NotMain {
			overload public function new() {}

			overload public function f(s:String) {}
			overload function f(i:Int) {}
		}

		class Main {
			static function main() {
				new NotMain().f({-1-})
			}
		}
	**/
	function testMemberVisibility() {
		sigEq(0, [["s:String"]], signature(pos(1)));
	}

	/**
		class NotMain {
			overload static public function f(s:String) {}
			overload static function f(i:Int) {}
		}

		class Main {
			static function main() {
				NotMain.f({-1-})
			}
		}
	**/
	function testStaticVisibility() {
		sigEq(0, [["s:String"]], signature(pos(1)));
	}
}
