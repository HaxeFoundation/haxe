package cases.display.issues;

class Issue9115 extends DisplayTestCase {
	function test(_) {
		var content = getTemplate("issues/Issue9115/A.hx");
		var markers = Markers.parse(content);
		vfs.putContent("A.hx", markers.source);
		runHaxe(["--no-output", "A"]);
		runHaxeJson([], DisplayMethods.Hover, {
			file: new FsPath("A.hx"),
			offset: markers.offset(1)
		});
		var result = parseHover();
		Assert.equals("A", result.result.item.type.args.path.typeName /* lol */);
	}
}