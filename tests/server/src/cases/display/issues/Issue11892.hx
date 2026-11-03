package cases.display.issues;

import haxe.display.FsPath;
import haxe.display.Display;

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
		Assert.isNull(runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: transform.markers[1]}));

		runHaxe(["--no-output"].concat(args));

		// Previously was pointing to @:value
		Assert.isNull(runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: transform.markers[2]}));

		runHaxe(["--no-output"].concat(args));

		// Previously was pointing to @:pure(expect)
		// But previously was also giving an error..
		Assert.isNull(runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: transform.markers[3]}));

		runHaxe(["--no-output"].concat(args));

		// Previously was pointing to @:directlyUsed
		Assert.isNull(runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: transform.markers[4]}));
	}
}
