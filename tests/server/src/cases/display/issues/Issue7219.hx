package cases.display.issues;

class Issue7219 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				if ([].{-1-}
			}
		}
	**/
	function testIf(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		Assert.isTrue(result.result.items.length > 0);
	}

	/**
		class Main {
			static function main() {
				for ([].{-1-}
			}
		}
	**/
	function testFor(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		Assert.isTrue(result.result.items.length > 0);
	}

	/**
		class Main {
			static function main() {
				while ([].{-1-}
			}
		}
	**/
	function testWhile(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		Assert.isTrue(result.result.items.length > 0);
	}

	/**
		class Main {
			static function main() {
				do [].{-1-}
			}
		}
	**/
	function testDoWhile1(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		Assert.isTrue(result.result.items.length > 0);
	}

	/**
		class Main {
			static function main() {
				do {

				} while ([].{-1-}
			}
		}
	**/
	function testDoWhile2(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		Assert.isTrue(result.result.items.length > 0);
	}
}
