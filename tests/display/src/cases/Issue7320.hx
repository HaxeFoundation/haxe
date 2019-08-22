package cases;

class Issue7320 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var f{-1-}
			}
		}
	**/
	@:func
	function test1() {
		eq(true, noCompletionPoint(toplevel.bind((pos(1)))));
	}

	/**
		class Main {
			static function main() {
				var f = {-1-}
			}
		}
	**/
	@:func
	function test2() {
		eq(true, hasToplevel(toplevel(pos(1)), "type", "Float"));
	}

	/**
		class Main {
			static function main() {
				var f, l{-1-}
			}
		}
	**/
	@:func
	function test3() {
		eq(true, noCompletionPoint(toplevel.bind((pos(1)))));
	}

	/**
		class Main {
			static function main() {
				var f = "foo", l{-1-}
			}
		}
	**/
	@:func
	function test4() {
		eq(true, noCompletionPoint(toplevel.bind((pos(1)))));
	}

	/**
		class Main {
			static function main() {
				var f, l{-1-} = "foo"
			}
		}
	**/
	@:func
	function test5() {
		eq(true, noCompletionPoint(toplevel.bind((pos(1)))));
	}
}
