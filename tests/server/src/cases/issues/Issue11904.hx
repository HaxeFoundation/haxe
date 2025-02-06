package cases.issues;

import haxe.display.Diagnostic;

class Issue11904 extends TestCase {
	function test(_) {
		vfs.putContent("Issue11904.hx", getTemplate("issues/Issue11904.hx"));
		var args = ["-main", "Issue11904", "--js", "no.js", "--no-output"];
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Issue11904.hx")});
		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("Issue11904.hx")}, res -> {
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
}
