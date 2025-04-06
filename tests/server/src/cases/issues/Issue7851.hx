package cases.issues;

import utest.Assert;

class Issue7851 extends TestCase {
	function test(_) {
		vfs.putContent("a.js", getTemplate("issues/Issue7851/a.js"));
		vfs.putContent("Main.hx", getTemplate("issues/Issue7851/Main.hx"));
		var args = ["-main", "Main", "--js", "test.js"];

		runHaxe(args);
		assertSuccess();
		var initialContents = vfs.getContent("test.js");
		Assert.isTrue(initialContents.contains("this is a.js"));

		runHaxe(args);
		assertSuccess();
		var contents = vfs.getContent("test.js");
		Assert.equals(initialContents, contents);
	}
}
