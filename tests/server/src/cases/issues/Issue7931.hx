package cases.issues;

class Issue7931 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue7931/Main.hx"));
		var args = ["-main", "Main"];
		runHaxe(args);
		assertErrorMessage("Local variable s used without being initialized");
		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}, res -> {
			Assert.equals("Local variable s used without being initialized", res[0].diagnostics[0].args);
		});
	}
}
