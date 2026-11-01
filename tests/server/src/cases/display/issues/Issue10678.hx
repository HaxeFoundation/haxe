package cases.display.issues;

class Issue10678 extends DisplayTestCase {
	#if todo // needs to be tested differently
	function test(_) {
		vfs.putContent("Macro.hx", getTemplate("issues/Issue10678/Macro.hx"));
		vfs.putContent("HelloWorld.hx", getTemplate("HelloWorld.hx"));
		var args = ["--main HelloWorld", "--js", "js.js", "--macro", "Macro.init()"];
		// This is very facepalm
		var results:Array<Dynamic> = cast runHaxeJson(args, DisplayMethods.FindReferences, {file: new FsPath("HelloWorld.hx"), offset: 0});
		for (i => result in results) {
			Assert.notContains(result, results.slice(i + 1));
		}
	}
	#end
}
