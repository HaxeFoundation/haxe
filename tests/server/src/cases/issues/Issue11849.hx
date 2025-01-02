package cases.issues;

class Issue11849 extends TestCase {
	function test(_) {
		var content = getTemplate("issues/Issue11849/Main.hx");
		var transform = Markers.parse(content);
		vfs.putContent("Main.hx", transform.source);

		var args = ["-main", "Main"];
		runHaxe(args);
		assertSuccess();

		runHaxeJsonCb(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: transform.offset(1)}, res -> {
			switch (res.item.kind) {
				case Local:
					Assert.equals("bar", res.item.args.name);

				case kind:
					Assert.fail("unexpected item kind: " + kind);
			}
		});
		assertSuccess();
	}
}
