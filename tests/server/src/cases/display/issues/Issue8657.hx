package cases.display.issues;

class Issue8657 extends DisplayTestCase {
	/**class Main { static function main() { var x:{-1-}stream{-2-} } }**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(2), wasAutoTriggered: true});
		var result = parseCompletion();
		var r = result.result;
		Assert.equals("stream", r.filterString);
		Assert.equals(offset(1), r.replaceRange.start.character);
		Assert.equals(offset(2), r.replaceRange.end.character);
	}
}