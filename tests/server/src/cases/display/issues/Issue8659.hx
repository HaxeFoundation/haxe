package cases.display.issues;

class Issue8659 extends DisplayTestCase {
	/**class Main extends {-1-}StreamTokenizer{-2-} { }**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(2), wasAutoTriggered: true});
		var r = result;
		Assert.equals("StreamTokenizer", r.filterString);
		Assert.equals(offset(1), r.replaceRange.start.character);
		Assert.equals(offset(2), r.replaceRange.end.character);
	}
}