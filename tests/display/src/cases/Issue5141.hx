package cases;

class Issue5141 extends DisplayTestCase {
	/**
	typedef MyHandler = Int->String->Void

	class Some {
		function main() {
			var a:MyHandler;
			a{-1-};
			a({-2-}
		}
	}
	**/
	function testTypedef() {
		eq("cases.MyHandler", type(pos(1)));
		arrayEq(["Int -> String -> Void"], signatures(pos(2)));
	}

	/**
	@:callable
	abstract MyCallable(Int->String->Void) {}

	class Some {
		function main() {
			var a:MyCallable;
			a{-1-};
			a({-2-}
		}
	}
	**/
	function testAbstract() {
		eq("cases.MyCallable", type(pos(1)));
		arrayEq(["Int -> String -> Void"], signatures(pos(2)));
	}
}
