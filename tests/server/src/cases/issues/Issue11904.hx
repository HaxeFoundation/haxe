package cases.issues;

import haxe.display.Diagnostic;

class Issue11904 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue11904/Main.hx"));
		var args = ["-main", "Main", "--js", "no.js", "--no-output"];
		runHaxe(args);
		assertSuccess();
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}, res -> {
			Assert.equals(1, res.length);
			Assert.equals(2, res[0].diagnostics.length);

			function check<T>(d:Diagnostic<T>) {
				switch (d.kind) {
					case ReplaceableCode:
						Assert.equals("Unused variable", d.args.description);

					case _:
						// trace(d);
						Assert.fail("Unexpected diagnostics kind: " + d.kind);
				}
			}

			var diag = res[0].diagnostics;
			for (d in diag) check(d);
		});
	}

	function testUntypedCast(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue11904/Main1.hx"));
		vfs.putContent("MyStringTools.hx", getTemplate("issues/Issue11904/MyStringTools.hx"));
		var args = ["-main", "Main", "--js", "no.js", "--no-output"];
		runHaxe(args);
		assertErrorMessage("Null safety: Cannot unify String with { charCodeAt : Int -> Int }");
	}
}
