package cases.display.issues;

class Issue8651 extends DisplayTestCase {
	/**class Main { static function main() { {-1-}buffer{-2-} } }**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(2), wasAutoTriggered: true});
		var result = parseCompletion();
		var r = result.result;
		Assert.equals("buffer", r.filterString);
		Assert.equals(offset(1), r.replaceRange.start.character);
		Assert.equals(offset(2), r.replaceRange.end.character);
	}
}