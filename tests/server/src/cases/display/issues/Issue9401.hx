package cases.display.issues;

class Issue9401 extends DisplayTestCase {
	/**
		class Foo {
			/** a0 **\/ public var a(get, set): Int;
			/** a1 **\/ function get_a() return 0;
			/** a2 **\/ function set_a(a: Int) return a;
			public function new() {}
		}

		@:forward abstract Bar(Foo) from Foo {
			/** b0 **\/ public var b(get, set): Int;
			/** b1 **\/ function get_b() return this.a;
			/** b2 **\/ function set_b(b: Int) return this.a = b;
		}

		class Main {
			public static function main() {
				var foo: Foo = new Foo();
				var bar: Bar = foo;

				foo.{-1-}a;
				foo.{-2-}a = 0;
				bar.{-3-}a;
				bar.{-4-}a = 0;
				bar.{-5-}b;
				bar.{-6-}b = 0;
			}
		}
	**/
	function testCatch_noTypeHint(_) {
		for (i in 1...5) {
			var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(i)});
			Assert.equals("a0", StringTools.trim(result.item.args.field.doc));
		}
		for (i in [5, 6]) {
			var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(i)});
			Assert.equals("b0", StringTools.trim(result.item.args.field.doc));
		}
	}
}
