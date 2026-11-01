package cases.issues;

class Issue11516 extends TestCase {
	function testClass(_) {
		vfs.putContent("Importson.hx", getTemplate("issues/Issue11516/Importson.hx"));
		var args = ["Importson", "--interp"];
		runHaxe(args);
		Assert.equals(0, runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Importson.hx")}).length);
	}
}
