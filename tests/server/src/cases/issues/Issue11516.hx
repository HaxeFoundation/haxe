package cases.issues;

class Issue11516 extends TestCase {
	function testClass(_) {
		vfs.putContent("Importson.hx", getTemplate("issues/Issue11516/Importson.hx"));
		var args = ["Importson", "--interp"];
		runHaxe(args);
		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("Importson.hx")}, res -> {
			Assert.equals(0, res.length);
		});
	}
}
