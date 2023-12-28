package cases;

class Metadata extends DisplayTestCase {
	/**
		@{-1-}
		class Some { }
	**/
	function testCompletion() {
		eq(true, hasPath(fields(pos(1)), "@:generic"));
	}

	/**
		@{-1-}:{-2-}
		class Some { }
	**/
	function testCompletion2() {
		eq(true, hasPath(fields(pos(1)), "@:generic"));
		eq(true, hasPath(fields(pos(2)), "@:generic"));
	}

	/**
		@{-1-}:{-2-}gen{-3-}oric
		class Some { }
	**/
	function testCompletion3() {
		eq(true, hasPath(fields(pos(1)), "@:generic"));
		eq(true, hasPath(fields(pos(2)), "@:generic"));
		eq(true, hasPath(fields(pos(3)), "@:generic"));
	}

	/**
		@:gen{-1-}eric
		class Some { }
	**/
	function testHover() {
		eq("Marks a class or class field as generic so each type parameter combination generates its own type/field.", metadataDoc(pos(1)));
	}

	/**
		class {-1-}SomeOther{-2-} { }

		@:myMeta(Som{-3-}eOther)
		class Some { }
	**/
	function testArgs() {
		eq(range(1, 2), position(pos(3)));
		eq("Class<cases.SomeOther>", type(pos(3)));
	}

	/**
		class Main {
			public static function main() { @{-1-} }
		}
	**/
	function testExpression1() {
		eq(true, hasPath(fields(pos(1)), "@:generic"));
	}

	/**
		class Main {
			public static function main() { @{-1-}:{-2-} }
		}
	**/
	function testExpression2() {
		eq(true, hasPath(fields(pos(1)), "@:generic"));
		eq(true, hasPath(fields(pos(2)), "@:generic"));
	}

	/**
		class Main {
			public static function main() { @{-1-}:{-2-}gene{-3-}ric }
		}
	**/
	function testExpression3() {
		eq(true, hasPath(fields(pos(1)), "@:generic"));
		eq(true, hasPath(fields(pos(2)), "@:generic"));
		eq(true, hasPath(fields(pos(3)), "@:generic"));
	}

	/**
		class Main {
			public static function main() { @{-1-}:{-2-}gene{-3-}ric null; }
		}
	**/
	function testExpression4() {
		eq(true, hasPath(fields(pos(1)), "@:generic"));
		eq(true, hasPath(fields(pos(2)), "@:generic"));
		eq(true, hasPath(fields(pos(3)), "@:generic"));
	}

	/**
		class Main {
			public static function main() { @{-1-}:{-2-}gene{-3-}ric }
		}
	**/
	function testExpression5() {
		eq("Marks a class or class field as generic so each type parameter combination generates its own type/field.", metadataDoc(pos(1)));
		eq("Marks a class or class field as generic so each type parameter combination generates its own type/field.", metadataDoc(pos(2)));
		eq("Marks a class or class field as generic so each type parameter combination generates its own type/field.", metadataDoc(pos(3)));
	}

	/**
		class Main {
			public static function main() { @{-1-}:{-2-}gene{-3-}ric null; }
		}
	**/
	function testExpression6() {
		eq("Marks a class or class field as generic so each type parameter combination generates its own type/field.", metadataDoc(pos(1)));
		eq("Marks a class or class field as generic so each type parameter combination generates its own type/field.", metadataDoc(pos(2)));
		eq("Marks a class or class field as generic so each type parameter combination generates its own type/field.", metadataDoc(pos(3)));
	}

	/**
		#if !macro
		@:build(cases.Metadata.Main.build())
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
	function test9853() {
		eq(true, hasPath(fields(pos(1)), "@:generic"));
	}

	/**
		@{-1-}
		class Main {
			static function main() {}
		}
		@{-2-}
		class Test {}

	**/
	function test7864() {
		eq(true, hasPath(fields(pos(1)), "@:generic"));
		eq(true, hasPath(fields(pos(2)), "@:generic"));
	}

	/**
		function main() {
			var @{-1-}
		}
	**/
	function test9639_1() {
		eq(true, hasPath(fields(pos(1)), "@:generic"));
	}

	/**
		function main() {
			var @{-1-} local
		}
	**/
	function test9639_2() {
		eq(true, hasPath(fields(pos(1)), "@:generic"));
	}

	/**
		function main() {
			var @{-1-} local : Type
		}
	**/
	function test9639_3() {
		eq(true, hasPath(fields(pos(1)), "@:generic"));
	}

	/**
		function main() {
			var @{-1-} local =
		}
	**/
	function test9639_4() {
		eq(true, hasPath(fields(pos(1)), "@:generic"));
	}

	/**
		function main() {
			var @{-1-} local = 10
		}
	**/
	function test9639_5() {
		eq(true, hasPath(fields(pos(1)), "@:generic"));
	}
}
