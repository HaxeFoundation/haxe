package cases;

class Issue9824 extends DisplayTestCase {
	/**
		extern class Foo {
			static overload function foo(a:Int):Int;
			static overload function foo(a:Int, b:Int):String;
			static overload function foo(a:String):Int;
		}

		class Main {
			public static function main() {
				var {-1-}x = Foo.foo(1, 2);
			}
		}
	**/
	function test() {
		eq("String", type(pos(1)));
	}
}
