package cases.display.issues;

import TestCase;
import haxe.Exception;

class Issue8194 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				switch ("p") {
					case "p"{-1-}
						"foo";
				}
			}
		}
	**/
	function test(_) {
		try {
			runHaxeJson([], DisplayMethods.Completion, {
				file: file,
				offset: offset(1),
				wasAutoTriggered: true
			});
			Assert.fail();
		} catch (e:TestException) {
			Assert.equals("No completion point", e.message);
		}
	}
}
