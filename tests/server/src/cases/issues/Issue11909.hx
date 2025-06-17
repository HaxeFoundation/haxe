package cases.issues;

import haxe.display.Diagnostic;

class Issue11909 extends TestCase {
	function test(_) {
		var content = getTemplate("issues/Issue11909/Main.hx");
		var transform = Markers.parse(content);
		vfs.putContent("Main.hx", transform.source);

		var args = ["-main", "Main"];
		runHaxe(args);
		assertSuccess();

		runHaxeJsonCb(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: transform.offset(1)}, res -> {
			switch (res.item.kind) {
				case Local:
					Assert.equals("int", res.item.args.name);
					Assert.equals("Int", res.item.args.type.args.path.typeName);

				case kind:
					Assert.fail("unexpected item kind: " + kind);
			}
		});
		assertSuccess();
	}
}
