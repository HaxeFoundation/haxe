package cases.display.issues;

import TestCase;

class Issue6068 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var a:{i:Int};
				a({-1-});

				Main({-2-});
			}
		}
	**/
	function test(_) {
		try {
			runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(1), wasAutoTriggered: false});
			Assert.fail();
		} catch (e:TestException) {
			Assert.isTrue(e.message.indexOf("Not a callable type") >= 0);
		}

		try {
			runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(2), wasAutoTriggered: false});
			Assert.fail();
		} catch (e:TestException) {
			// TESTTODO: should be "Not a callable type" for offset(2) like for offset(1)
			Assert.pass();
		}
	}
}
