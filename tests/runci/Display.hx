package runci;

import haxe.io.Path;
import haxe.macro.Compiler.Platform;
import sys.FileSystem;

class Display {
	static public function maybeRunDisplayTests(target:Platform) {
		final target = target.getName();
		final pack = Path.join([Config.displayDir, "src", "cases", target.toLowerCase()]);
		if (FileSystem.exists(pack)) {
			System.changeDirectory(Config.displayDir);
			System.haxelibInstallGit("Simn", "haxeserver");
			System.runCommand("haxe", ["build.hxml", "-D", "display.protocol=xml", "-D", 'display.target=$target']);
			System.runCommand("haxe", ["build.hxml", "-D", "display.protocol=jsonrpc", "-D", 'display.target=$target']);
		}
	}
}
