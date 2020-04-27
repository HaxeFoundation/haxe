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
		// TODO: We should use the markers, but I forgot how to get lines and characters from offsets
		// Also That Assert.same doesn't work
		Assert.equals(9, result[0].range.start.line);
		Assert.equals(19, result[0].range.start.character);
		Assert.equals(9, result[0].range.end.line);
		Assert.equals(23, result[0].range.end.character);
		// Assert.same([
		// 	{
		// 		range: {
		// 			start: {line: 9, character: 1},
		// 			end: {line: 11, character: 2}
		// 		}
		// 	}
		// ], result);
	}
}