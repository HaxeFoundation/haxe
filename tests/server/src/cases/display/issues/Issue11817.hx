package cases.display.issues;

class Issue11817 extends DisplayTestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue11817/MainBefore.hx"));
		vfs.putContent("Utils.hx", getTemplate("issues/Issue11817/Utils.hx"));
		runHaxe(["--main", "Main"]);
		var mainHx = Marker.extractMarkers(getTemplate("issues/Issue11817/MainAfter.hx"));
		vfs.putContent("Main.hx", mainHx.source);
		runHaxeJson([], ServerMethods.Invalidate, {file: file});
		runHaxeJson([], DisplayMethods.Completion, {
			file: file,
			offset: mainHx.markers[1],
			wasAutoTriggered: true
		});
		var result = parseCompletion().result;
		Assert.equals(1, result.items.length);
		Assert.equals('foo', result.items[0].args.field.name);

		vfs.putContent("Main.hx", getTemplate("issues/Issue11817/MainError.hx"));
		runHaxe(["--main", "Main"]);
		assertErrorMessage("This field cannot be accessed explicitly");
	}
}
