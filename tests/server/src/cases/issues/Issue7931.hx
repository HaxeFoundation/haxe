package cases.issues;

class Issue7931 extends TestCase {
	function test(_) {
		var content = getTemplate("issues/Issue7931/Main.hx");
		var transform = Markers.parse(content);

		vfs.putContent("Main.hx", transform.source);
		var args = ["-main", "Main"];
		runHaxe(args);
		assertErrorMessage("Local variable s used without being initialized");
		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}, res -> {
			Assert.equals("Local variable s used without being initialized", res[0].diagnostics[0].args);
			Assert.same(transform.range(1,2), res[0].diagnostics[0].range);
		});
	}
}
