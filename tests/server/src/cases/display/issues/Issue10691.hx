package cases.display.issues;

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
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		Assert.equals(0, result.result.items.length);
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
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		Assert.equals(0, result.result.items.length);
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
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		Assert.equals(0, result.result.items.length);
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
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		Assert.equals(0, result.result.items.length);
	}
}
