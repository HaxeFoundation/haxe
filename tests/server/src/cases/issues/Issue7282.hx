package cases.issues;

import haxe.display.Diagnostic;
import haxe.display.Position.Range;

class Issue7282 extends TestCase {
	function test(_) {
		var content = getTemplate("issues/Issue7282/Main.hx");
		var transform = Markers.parse(content);

		vfs.putContent("Main.hx", transform.source);
		var args = ["-main", "Main"];
		runHaxe(args);
		assertSuccess();
		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}, res -> {
			var arg:{description:String, range:Range} = res[0].diagnostics[0].args;
			Assert.equals("Unused variable", arg.description);
			Assert.same(transform.range(1,2), res[0].diagnostics[0].range);
			Assert.same(transform.range(1,2), arg.range);
		});
	}
}
