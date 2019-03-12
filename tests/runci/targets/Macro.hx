package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;
import runci.targets.Cs.*;
import runci.targets.Python.*;

class Macro {
	static public function run(args:Array<String>) {
		runCommand("haxe", ["compile-macro.hxml"].concat(args));

		haxelibInstallGit("Simn", "haxeserver");

		changeDirectory(displayDir);
		haxelibInstallGit("Simn", "haxeserver");
		runCommand("haxe", ["build.hxml"]);
	}
}