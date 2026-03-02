package cases.issues;

class Issue12018 extends TestCase {
	function test(_) {
		var content = getTemplate("issues/Issue12018/Main.hx");
		var transform = Markers.parse(content);

		vfs.putContent("Main.hx", transform.source);
		vfs.putContent("import.hx", getTemplate("issues/Issue12018/import.hx"));
		var args = ["-main", "Main"];
		runHaxe(args);
		assertSuccess();

		Assert.equals("Main", runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: transform.offset(1)}).item.args.path.typeName);
	}
}
