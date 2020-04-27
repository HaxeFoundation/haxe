package cases.display.issues;

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
		runHaxeJson([], DisplayMethods.Completion, {
			file: file,
			offset: offset(1),
			wasAutoTriggered: true
		});
		var result = parseCompletion();
		Assert.equals(null, result.result);
	}
}