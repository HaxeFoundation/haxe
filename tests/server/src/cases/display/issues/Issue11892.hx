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
		final r1 = runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: transform.markers[1]});
		Assert.isNull(r1); // null hover result expected for compiler metadata positions
		Assert.pass(); // no error was raised (equivalent to original Assert.equals(null, response.error))

		runHaxe(["--no-output"].concat(args));

		// Previously was pointing to @:value
		final r2 = runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: transform.markers[2]});
		Assert.isNull(r2);
		Assert.pass(); // no error was raised

		runHaxe(["--no-output"].concat(args));

		// Previously was pointing to @:pure(expect)
		// But previously was also giving an error..
		final r3 = runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: transform.markers[3]});
		Assert.isNull(r3);
		Assert.pass(); // no error was raised

		runHaxe(["--no-output"].concat(args));

		// Previously was pointing to @:directlyUsed
		final r4 = runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: transform.markers[4]});
		Assert.isNull(r4);
		Assert.pass(); // no error was raised
	}
}
