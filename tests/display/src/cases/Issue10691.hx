package cases;

class Issue10691 extends DisplayTestCase {
	/**
		class Main {
			static public function main() {
				function hello() {}
				function {-1-}
			}
		}
	**/
	function test1() {
		eq(true, noCompletionPoint(toplevel.bind((pos(1)))));
	}

	/**
		class Main {
			static public function main() {
				function hello() {}
				function a{-1-}
			}
		}
	**/
	function test2() {
		eq(true, noCompletionPoint(toplevel.bind((pos(1)))));
	}

	/**
		class Main {
			static public function main() {
				function hello() {}
				function a{-1-}b
			}
		}
	**/
	function test3() {
		eq(true, noCompletionPoint(toplevel.bind((pos(1)))));
	}

	/**
		class Main {
			static public function main() {
				function hello() {}
				function a{-1-}b()
			}
		}
	**/
	function test4() {
		eq(true, noCompletionPoint(toplevel.bind((pos(1)))));
	}

	/**
		class Main {
			static public function main() {
				function hello() {}
				function ab{-1-}()
			}
		}
	**/
	function test5() {
		eq(true, noCompletionPoint(toplevel.bind((pos(1)))));
	}
}
