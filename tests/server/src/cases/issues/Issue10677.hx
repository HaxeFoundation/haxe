package cases.issues;

class Issue10677 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue10677/Main.hx"));
		vfs.putContent("Polygon.hx", getTemplate("issues/Issue10677/Polygon.hx"));
		vfs.putContent("Polygons.hx", getTemplate("issues/Issue10677/Polygons.hx"));
		vfs.putContent("PolygonCollider.hx", getTemplate("issues/Issue10677/PolygonCollider.hx"));
		var args = ["--main", "Main.hx", "--no-output"];
		runHaxe(args);
		runHaxeJson(args, ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxe(args);
		Assert.isFalse(lastResult.hasError);
	}
}
