package cases.display.issues;

class Issue8438 extends DisplayTestCase {
	/**class Main {
	static function main() {
		" ".char{-1-}
	}
}**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {
			file: file,
			offset: offset(1),
			wasAutoTriggered: true
		});
		var result = parseCompletion();
		Assert.equals(6, result.result.replaceRange.start.character);
		Assert.equals(10, result.result.replaceRange.end.character);
	}
}