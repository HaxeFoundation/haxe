package cases.display.issues;

class Issue8805 extends DisplayTestCase {
	function testIssue8805_gotoAbstractPropertyWithInlineGetter(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue8805/Main.hx"));
		var args = ["-main", "Main"];
		runHaxeJson(args, DisplayMethods.GotoDefinition, {file: file, offset: 56});
		var result = parseGotoTypeDefinition();
		if (result.result.length == 0) {
			Assert.fail('display/definition failed');
		} else {
			Assert.same({"start": {"line": 7, "character": 12}, "end": {"line": 7, "character": 15}}, result.result[0].range);
		}
	}
}