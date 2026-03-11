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
		check("get", runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: transform.offset(1)}));
		check("set", runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: transform.offset(2)}));
		assertSuccess();
	}
}
