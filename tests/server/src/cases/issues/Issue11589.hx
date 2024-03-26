package cases.issues;

class Issue11589 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue11589/Main.hx"));
		var args = ["--main", "Main.hx", "--no-output"];
		runHaxe(args);
		runHaxe(args);
		Assert.isFalse(lastResult.hasError);
	}

	function testNestedField(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue11589/Main1.hx"));
		var args = ["--main", "Main.hx", "--no-output"];
		runHaxe(args);
		runHaxe(args);
		Assert.isFalse(lastResult.hasError);
	}
}
