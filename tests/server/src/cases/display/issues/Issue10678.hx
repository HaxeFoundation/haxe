package cases.display.issues;

import haxe.Json;

class Issue10678 extends DisplayTestCase {
	function test(_) {
		vfs.putContent("Macro.hx", getTemplate("issues/Issue10678/Macro.hx"));
		vfs.putContent("HelloWorld.hx", getTemplate("HelloWorld.hx"));
		var args = ["--main HelloWorld", "--js", "js.js", "--macro", "Macro.init()"];
		// This is very facepalm
		runHaxeJson(args, DisplayMethods.FindReferences, {file: new FsPath("HelloWorld.hx"), offset: 0});
		var results:Array<String> = Json.parse(lastResult.stderr).result.result;
		for (i => result in results) {
			Assert.notContains(result, results.slice(i + 1));
		}
	}
}
