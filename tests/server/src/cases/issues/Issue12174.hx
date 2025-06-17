package cases.issues;

import haxe.display.Diagnostic;

class Issue12174 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue12174.hx"));
		var args = ["-main", "Main"];
		runHaxe(args);
		assertSuccess();
		Assert.equals(1, messages.filter(m -> m.contains("WDeprecated")).length);

		runHaxe(args);
		assertSuccess();
		Assert.equals(1, messages.filter(m -> m.contains("WDeprecated")).length);
	}
}
