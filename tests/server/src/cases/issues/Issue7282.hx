package cases.issues;

class Issue7282 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue7282/Main.hx"));
		var args = ["-main", "Main"];
		runHaxe(args);
		assertSuccess();
		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}, res -> {
			Assert.equals("Unused variable", (cast res[0].diagnostics[0].args).description);
		});
	}
}
