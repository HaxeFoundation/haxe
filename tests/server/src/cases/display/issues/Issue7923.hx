package cases.display.issues;

class Issue7923 extends DisplayTestCase {
	function test(_) {
		vfs.putContent("TreeItem.hx", getTemplate("issues/Issue7923/TreeItem.hx"));
		var content = getTemplate("issues/Issue7923/Main.hx");
		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);
		runHaxeJson([], DisplayMethods.Completion, {
			file: new FsPath("Main.hx"),
			offset: transform.markers[1],
			wasAutoTriggered: true
		});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch (item.kind) {
			case EnumAbstractField: item.args.field.name == "Collapsed";
			case _: false;
		});
	}
}