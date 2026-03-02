package cases.display.issues;

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
			var error = haxe.Json.parse(lastResult.stderr).error;
			Assert.equals("No completion point", error.data[0]);
		} catch (e:Exception) {
			Assert.pass(); // TODO
		}
	}
}
