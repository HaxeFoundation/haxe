package cases.issues;

class Issue11757 extends TestCase {
	function test(_) {
		var content = getTemplate("issues/Issue11757/Main.hx");
		var transform = Markers.parse(content);
		vfs.putContent("Main.hx", transform.source);

		var args = ["-main", "Main"];
		runHaxe(args);
		assertSuccess();
		function check(name:String, res:HoverDisplayItemOccurence<Dynamic>) {
			switch (res.item.kind) {
				case ClassField:
					Assert.equals(name, res.item.args.field.name);

				case kind:
					Assert.fail("unexpected item kind: " + kind);
			}
		}
		runHaxeJsonCb(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: transform.offset(1)}, res -> check("get", res));
		runHaxeJsonCb(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: transform.offset(2)}, res -> check("set", res));
		assertSuccess();
	}
}
