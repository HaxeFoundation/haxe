package cases.display;

class StaticExtension extends DisplayTestCase {
	/**

		using Main.MyStaticExtension;
		class Something {
			static function test() {
				var map = ["a" => 1];
				map.{-1-}
			}
		}

		class MyStaticExtension {
			static public function doSomething(sm:haxe.ds.StringMap<Int>):Void { }
			static public function doSomethingElse(sm:Map<String, Int>):Void { }
		}

	**/
	function test1(_) {
		var f = fields(1);
		eq(true, hasField(f, "doSomething", "() -> Void"));
		eq(true, hasField(f, "doSomethingElse", "() -> Void"));
	}

	/**

		using Main.MyStaticExtension;
		class Something {
			static function test() {
				var map = new haxe.ds.StringMap();
				map.{-1-}
			}
		}

		class MyStaticExtension {
			static public function doSomething(sm:haxe.ds.StringMap<Int>):Void { }
			static public function doSomethingElse(sm:Map<String, Int>):Void { }
		}

	**/
	function test2(_) {
		var f = fields(1);
		eq(true, hasField(f, "doSomething", "() -> Void"));
		eq(true, hasField(f, "doSomethingElse", "() -> Void"));
	}

	/**
		using Main;

		class Overload1 {
			public static function test(o:String):Void { }
		}

		class Overload2 {
			public static function test(o:String, i:Int):Void { }
		}

		class Main {
			static public function main() {
				"".{-1-}
			}
		}
	**/
	function testIssue8584(_) {
		var f = fields(1);
		eq(true, hasField(f, "test", "(i : Int) -> Void"));
		eq(false, hasField(f, "test", "() -> Void"));
	}
}
