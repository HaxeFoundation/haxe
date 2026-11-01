package cases.display.issues;

class Issue9824 extends DisplayTestCase {
	/**
		extern class Foo {
			static overload function foo(a1:Int):Int;
			static overload function foo(a2:Int, b:Int):String;
		}

		class Main {
			public static function main() {
				var myNull = null;
				var xxx = Foo.foo(12{-1-}3, 45{-2-}6);
			}
		}
	**/
	function test(_) {
		var args = ["-main", "Main", "--no-output", "--jvm", "no.jar"];
		runHaxe(args);
		var result = runHaxeJson(args, DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("a2", result.expected.name.name);

		var result = runHaxeJson(args, DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.equals("b", result.expected.name.name);
	}
}
