package cases.display;

class Metadata extends DisplayTestCase {
	/**
		@{-1-}
		class Some { }
	**/
	function testCompletion(_) {
		eq(true, hasPath(fields(1), "@:generic"));
	}

	/**
		@{-1-}:{-2-}
		class Some { }
	**/
	function testCompletion2(_) {
		eq(true, hasPath(fields(1), "@:generic"));
		eq(true, hasPath(fields(2), "@:generic"));
	}

	/**
		@{-1-}:{-2-}gen{-3-}oric
		class Some { }
	**/
	function testCompletion3(_) {
		eq(true, hasPath(fields(1), "@:generic"));
		eq(true, hasPath(fields(2), "@:generic"));
		eq(true, hasPath(fields(3), "@:generic"));
	}

	/**
		@:gen{-1-}eric
		class Some { }
	**/
	function testHover(_) {
		eq("Marks a class or class field as generic so each type parameter combination generates its own type/field.", metadataDoc(1));
	}

	/**
		class {-1-}SomeOther{-2-} { }

		@:myMeta(Som{-3-}eOther)
		class Some { }
	**/
	function testArgs(_) {
		Assert.same(range(1, 2), position(3));
		eq("Class<SomeOther>", type(3));
	}

	/**
		class Main {
			public static function main() { @{-1-} }
		}
	**/
	function testExpression1(_) {
		eq(true, hasPath(fields(1), "@:generic"));
	}

	/**
		class Main {
			public static function main() { @{-1-}:{-2-} }
		}
	**/
	function testExpression2(_) {
		eq(true, hasPath(fields(1), "@:generic"));
		eq(true, hasPath(fields(2), "@:generic"));
	}

	/**
		class Main {
			public static function main() { @{-1-}:{-2-}gene{-3-}ric }
		}
	**/
	function testExpression3(_) {
		eq(true, hasPath(fields(1), "@:generic"));
		eq(true, hasPath(fields(2), "@:generic"));
		eq(true, hasPath(fields(3), "@:generic"));
	}

	/**
		class Main {
			public static function main() { @{-1-}:{-2-}gene{-3-}ric null; }
		}
	**/
	function testExpression4(_) {
		eq(true, hasPath(fields(1), "@:generic"));
		eq(true, hasPath(fields(2), "@:generic"));
		eq(true, hasPath(fields(3), "@:generic"));
	}

	/**
		class Main {
			public static function main() { @{-1-}:{-2-}gene{-3-}ric }
		}
	**/
	function testExpression5(_) {
		eq("Marks a class or class field as generic so each type parameter combination generates its own type/field.", metadataDoc(1));
		eq("Marks a class or class field as generic so each type parameter combination generates its own type/field.", metadataDoc(2));
		eq("Marks a class or class field as generic so each type parameter combination generates its own type/field.", metadataDoc(3));
	}

	/**
		class Main {
			public static function main() { @{-1-}:{-2-}gene{-3-}ric null; }
		}
	**/
	function testExpression6(_) {
		eq("Marks a class or class field as generic so each type parameter combination generates its own type/field.", metadataDoc(1));
		eq("Marks a class or class field as generic so each type parameter combination generates its own type/field.", metadataDoc(2));
		eq("Marks a class or class field as generic so each type parameter combination generates its own type/field.", metadataDoc(3));
	}

	/**
		#if !macro
		@:build(Main.Main.build())
		#end
		class Main {
			#if !macro
			@{-1-}
			static var dummy = 123;

			static function main() {}
			#else
			static function build() {
				haxe.macro.Context.getBuildFields();
				return null;
			}
			#end
		}
	**/
	function test9853(_) {
		eq(true, hasPath(fields(1), "@:generic"));
	}

	/**
		@{-1-}
		class Main {
			static function main() {}
		}
		@{-2-}
		class Test {}

	**/
	function test7864(_) {
		eq(true, hasPath(fields(1), "@:generic"));
		eq(true, hasPath(fields(2), "@:generic"));
	}

	/**
		function main() {
			var @{-1-}
		}
	**/
	function test9639_1(_) {
		eq(true, hasPath(fields(1), "@:generic"));
	}

	/**
		function main() {
			var @{-1-} local
		}
	**/
	function test9639_2(_) {
		eq(true, hasPath(fields(1), "@:generic"));
	}

	/**
		function main() {
			var @{-1-} local : Type
		}
	**/
	function test9639_3(_) {
		eq(true, hasPath(fields(1), "@:generic"));
	}

	/**
		function main() {
			var @{-1-} local =
		}
	**/
	function test9639_4(_) {
		eq(true, hasPath(fields(1), "@:generic"));
	}

	/**
		function main() {
			var @{-1-} local = 10
		}
	**/
	function test9639_5(_) {
		eq(true, hasPath(fields(1), "@:generic"));
	}
}
