package cases.display.issues;

import TestCase;

class Issue10691 extends DisplayTestCase {
	/**
		class Main {
			static public function main() {
				function hello() {}
				function {-1-}
			}
		}
	**/
	function test1(_) {
		// TESTTODO: compiler should not return "No completion point" here; fix and assert proper result
		try {
			runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		} catch (e:TestException) {}
		Assert.pass();
	}

	/**
		class Main {
			static public function main() {
				function hello() {}
				function a{-1-}
			}
		}
	**/
	function test2(_) {
		// TESTTODO: compiler should not return "No completion point" here; fix and assert proper result
		try {
			runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		} catch (e:TestException) {}
		Assert.pass();
	}

	/**
		class Main {
			static public function main() {
				function hello() {}
				function a{-1-}b
			}
		}
	**/
	function test3(_) {
		// TESTTODO: compiler should not return "No completion point" here; fix and assert proper result
		try {
			runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		} catch (e:TestException) {}
		Assert.pass();
	}

	/**
		class Main {
			static public function main() {
				function hello() {}
				function a{-1-}b()
			}
		}
	**/
	function test4(_) {
		// TESTTODO: compiler should not return "No completion point" here; fix and assert proper result
		try {
			runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		} catch (e:TestException) {}
		Assert.pass();
	}
}
