package cases;

class Issue10004 extends DisplayTestCase {
	/**
		using Issue10004.Foo;

		class Main {
			static function main() {
				0.{-1-}
			}
		}

		class Foo {
			public static overload extern inline function foo():Void {}

			public static overload extern inline function foo(i:Int):Void {}
		}
	**/
	function testGama() {
		var fields = fields(pos(1));
		eq(true, hasField(fields, "foo", "() -> Void"));
	}

	/**
		using Issue10004.Tools;

		class Tools {
			public static extern inline overload function getOrZero<K>(map:Map<K,Int>, key:K):Int {
				var value = map.get(key);
				return if (value != null) value else 0;
			}

			public static extern inline overload function getOrZero<K>(map:Map<K,Float>, key:K):Float {
				var value = map.get(key);
				return if (value != null) value else 0.0;
			}

		}

		function main() {
			var m = ["a" => 1.1, "b" => 2.3];
			m.{-1-}
		}
	**/
	function testNotGama() {
		var fields = fields(pos(1));
		eq(true, hasField(fields, "getOrZero", "(key : String) -> Float"));
	}
}
