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
		eq("Marks a class or class field as generic so each type parameter combination generates its own type/field", metadataDoc(pos(1)));
	}

	/**
	{-1-}class SomeOther { }{-2-}

	@:myMeta(Som{-3-}eOther)
	class Some { }
	**/
	function testArgs() {
		eq(range(1, 2), position(pos(3)));
		eq("Class<cases.SomeOther>", type(pos(3)));
	}
}