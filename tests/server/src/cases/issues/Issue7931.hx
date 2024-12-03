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
			Assert.equals(1, res.length);
			Assert.equals(1, res[0].diagnostics.length);
			Assert.equals("Local variable s used without being initialized", res[0].diagnostics[0].args);
			Assert.same(transform.range(1,2), res[0].diagnostics[0].range);
		});
	}

	function testNullSafety(_) {
		var content = getTemplate("issues/Issue7931/Main1.hx");
		var transform = Markers.parse(content);

		vfs.putContent("Main.hx", transform.source);
		var args = ["-main", "Main", "-js", "out.js", "--no-output"];
		runHaxe(args);
		assertErrorMessage("Null safety: Cannot assign nullable value here.");
		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}, res -> {
			Assert.equals(1, res.length);
			Assert.equals(1, res[0].diagnostics.length);
			Assert.equals("Null safety: Cannot assign nullable value here.", res[0].diagnostics[0].args);
			Assert.same(transform.range(1,2), res[0].diagnostics[0].range);
		});
	}
}
