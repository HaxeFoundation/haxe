package cases;

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
	function testCatch_noTypeHint() {
		eq("a0", doc(pos(1)));
		eq("a0", doc(pos(2)));
		eq("a0", doc(pos(3)));
		eq("a0", doc(pos(4)));
		eq("b0", doc(pos(5)));
		eq("b0", doc(pos(6)));
	}
}
