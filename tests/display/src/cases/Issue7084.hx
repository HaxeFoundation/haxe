package cases;

class Issue7084 extends DisplayTestCase {
	/**
		enum abstract Foo(Int) {
			var Value = 0;
		}

		class Main {
			public static function main() {
				var f:Foo;
				f.{-1-}
			}
		}
	**/
	function test1() {
		eq(0, fields(pos(1)).length);
	}

	/**
		enum abstract Foo(Int) {
			var Value = 0;
		}

		class Main {
			public static function main() {
				Foo.{-1-};
			}
		}
	**/
	function test2() {
		eq(true, isField(fields(pos(1))[0], "Value"));
	}
}
