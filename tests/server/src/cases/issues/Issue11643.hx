package cases.issues;

class Issue11643 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue11643/Main.hx"));
		vfs.putContent("Foo.hx", getTemplate("issues/Issue11643/Foo.hx"));

		var args = ["-main", "Main", "--macro", "exclude('Foo')"];
		runHaxe(args);
		runHaxeJson(args, ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxe(args);
		Assert.isFalse(lastResult.hasError);
	}
}
