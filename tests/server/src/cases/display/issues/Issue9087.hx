package cases.display.issues;

class Issue9087 extends DisplayTestCase {
	function test(_) {
		var content = getTemplate("issues/Issue9087/A.hx");
		var markers = Markers.parse(content);
		vfs.putContent("A.hx", markers.source);
		var args = ["A", "-js", "main.js"];
		function parseGotoDefintion():GotoDefinitionResult {
			return haxe.Json.parse(lastResult.stderr).result;
		}
		runHaxeJson(args, DisplayMethods.GotoImplementation, {file: new FsPath("A.hx"), offset: markers.offset(1), contents: markers.source});
		var result = parseGotoDefintion().result;
		Assert.equals(1, result.length);
		Assert.same(markers.range(2, 3), result[0].range);
	}
}