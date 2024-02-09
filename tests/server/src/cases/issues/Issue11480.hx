package cases.issues;

import haxe.io.Path;

class Issue11480 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", "function main() {}");
		var args = [
			"-main",
			"Main",
			"-hl",
			"bin/out.hl",
			"-D",
			"no-compilation"
		];
		runHaxe(args);
		assertSuccess();

		var std = Path.removeTrailingSlashes(utils.macro.BuildHub.getStd());
		runHaxeJsonCb(args, DisplayMethods.Hover, {file: new FsPath('${std}/StdTypes.hx'), offset: 0}, (res) -> {
			// If we don't use the cb version, assertSuccess() below will pass even when request fails..
			Assert.isNull(res);
		});
		assertSuccess();
	}
}
