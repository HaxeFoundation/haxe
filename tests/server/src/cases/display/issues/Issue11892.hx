package cases.display.issues;

import haxe.Json;
import haxe.display.Display;

private typedef HoverResponse = {
	?error:{code:Int, message:String},
	?result:HoverResult
}

class Issue11892 extends DisplayTestCase {
	function testCompilerMetadata(_) {
		var content = getTemplate("issues/Issue11892/Main.hx");
		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);

		var args = ["--main", "Main", "-D", "analyzer-optimize", "--interp", "--dce=full"];
		// Needed to repro -4-
		args = args.concat(["-D", "disable-hxb-cache"]);
		runHaxe(["--no-output"].concat(args));

		// Previously was pointing to @:pure
		runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: transform.markers[1]});
		var response:HoverResponse = Json.parse(lastResult.stderr);
		Assert.equals(null, response.error);
		Assert.equals(null, response.result.result);

		runHaxe(["--no-output"].concat(args));

		// Previously was pointing to @:value
		runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: transform.markers[2]});
		var response:HoverResponse = Json.parse(lastResult.stderr);
		Assert.equals(null, response.error);
		Assert.equals(null, response.result.result);

		runHaxe(["--no-output"].concat(args));

		// Previously was pointing to @:pure(expect)
		// But previously was also giving an error..
		runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: transform.markers[3]});
		var response:HoverResponse = Json.parse(lastResult.stderr);
		Assert.equals(null, response.error);
		Assert.equals(null, response.result?.result);

		runHaxe(["--no-output"].concat(args));

		// Previously was pointing to @:directlyUsed
		runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: transform.markers[4]});
		var response:HoverResponse = Json.parse(lastResult.stderr);
		Assert.equals(null, response.error);
		Assert.equals(null, response.result.result);
	}
}
