package cases.display.issues;

import haxe.display.FsPath;
import haxe.display.Display;

class Issue11678 extends DisplayTestCase {
	function test(_) {
		vfs.putContent("ModuleFields.hx", getTemplate("issues/Issue11678/ModuleFields.hx"));
		var content = getTemplate("issues/Issue11678/Main.hx");
		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);

		var args = ["-main", "Main"];
		runHaxe(["--no-output"].concat(args));
		var response = runHaxeJson(args, DisplayMethods.Hover, {
			file: new FsPath("Main.hx"),
			offset: transform.markers[1]
		});

		Assert.equals("foo", response.item.args.field.name);
	}
}
