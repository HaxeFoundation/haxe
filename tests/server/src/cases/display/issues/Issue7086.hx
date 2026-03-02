package cases.display.issues;

import TestCase;

class Issue7086 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {
				~/{-1-}/;
			}
		}
	**/
	function test(_) {
		try {
			runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
			Assert.fail();
		} catch (e:TestException) {
			Assert.isTrue(e.message.indexOf("No completion point") >= 0);
		}
	}
}
