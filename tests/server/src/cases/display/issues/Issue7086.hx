package cases.display.issues;

class Issue7086 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {
				~/{-1-}/;
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		// Completion inside a regex literal should return empty or no completion
		Assert.equals(0, result.result.items.length);
	}
}
