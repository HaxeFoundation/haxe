package cases.display.issues;

class Issue7050 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				// {-1-}
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		// Completion inside a comment should return empty or no completion
		Assert.equals(0, result.result.items.length);
	}
}
