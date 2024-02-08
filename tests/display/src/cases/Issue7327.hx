package cases;

class Issue7327 extends DisplayTestCase {
	/**
		import haxe.ds.Option;

		class Main {
			public static function main() {
				var o:Option<Int> = None;
				switch (o) {
					case Some(v):{-1-} {-2-}
					case None:
				}
			}
		}
	**/
	function test1() {
		eq(true, hasToplevel(toplevel(pos(1)), "local", "v", "Int"));
		eq(true, hasToplevel(toplevel(pos(2)), "local", "v", "Int"));
	}

	/**
		import haxe.ds.Option;

		class Main {
			public static function main() {
				var o:Option<Int> = None;
				switch (o) {
					case Some(v):{-1-} {-2-}
				{-3-}}{-4-}
			}
		}
	**/
	function test2() {
		eq(true, hasToplevel(toplevel(pos(1)), "local", "v", "Int"));
		eq(true, hasToplevel(toplevel(pos(2)), "local", "v", "Int"));
		eq(true, hasToplevel(toplevel(pos(3)), "local", "v", "Int"));
		eq(false, hasToplevel(toplevel(pos(4)), "local", "v", "Int"));
	}

	/**
		import haxe.ds.Option;

		class Main {
			public static function main() {
				var o:Option<Int> = None;
				switch (o) {
					case Some(v):{-1-}
					default: {-2-}
				}
			}
		}
	**/
	function test3() {
		eq(true, hasToplevel(toplevel(pos(1)), "local", "v", "Int"));
		eq(false, hasToplevel(toplevel(pos(2)), "local", "v", "Int"));
	}

	/**
		import haxe.ds.Option;

		class Main {
			public static function main() {
				var o:Option<Int> = None;
				switch (o) {
					case Some(v):{-1-}
	**/
	function test4() {
		eq(true, hasToplevel(toplevel(pos(1)), "local", "v", "Int"));
	}
}
