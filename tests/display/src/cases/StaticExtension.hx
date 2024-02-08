package cases;

class StaticExtension extends DisplayTestCase {
	/**

		using cases.StaticExtension.MyStaticExtension;
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
	function test1() {
		var fields = fields(pos(1));
		eq(true, hasField(fields, "doSomething", "() -> Void"));
		eq(true, hasField(fields, "doSomethingElse", "() -> Void"));
	}

	/**

		using cases.StaticExtension.MyStaticExtension;
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
	function test2() {
		var fields = fields(pos(1));
		eq(true, hasField(fields, "doSomething", "() -> Void"));
		eq(true, hasField(fields, "doSomethingElse", "() -> Void"));
	}

	/**
		using cases.StaticExtension;

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
	function testIssue8584() {
		var fields = fields(pos(1));
		eq(true, hasField(fields, "test", "(i : Int) -> Void"));
		eq(false, hasField(fields, "test", "() -> Void"));
	}
}
