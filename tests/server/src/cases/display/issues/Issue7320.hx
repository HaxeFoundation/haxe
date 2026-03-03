package cases.display.issues;

import TestCase;

class Issue7320 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var f{-1-}
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
			static function main() {
				var f = {-1-}
			}
		}
	**/
	function test2(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "Float";
			case _: false;
		});
	}

	/**
		class Main {
			static function main() {
				var f, l{-1-}
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
			static function main() {
				var f = "foo", l{-1-}
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
