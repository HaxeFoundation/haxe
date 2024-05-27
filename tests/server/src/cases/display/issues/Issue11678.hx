package cases.display.issues;

import haxe.Json;
import haxe.display.Display;

typedef HoverResponse = {
	?error:{code:Int, message:String},
	?result:HoverResult
}

class Issue11678 extends DisplayTestCase {
	function test(_) {
		vfs.putContent("ModuleFields.js.hx", getTemplate("issues/Issue11678/ModuleFields.hx"));
		var content = getTemplate("issues/Issue11678/Main.hx");
		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);

		var args = ["-js", "test.js", "-main", "Main"];
		runHaxe(["--no-output"].concat(args));
		runHaxeJson(args, DisplayMethods.Hover, {
			file: new FsPath("Main.hx"),
			offset: transform.markers[1]
		});

		var response:HoverResponse = Json.parse(lastResult.stderr);
		Assert.equals(null, response.error);
		Assert.equals("foo", response.result.result.item.args.field.name);
	}
}
