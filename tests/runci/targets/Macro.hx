package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;
import runci.targets.Cs.*;
import runci.targets.Python.*;

class Macro {
	static public function run(args:Array<String>) {
		runCommand("haxe", ["compile-macro.hxml"].concat(args));

		haxelibInstallGit("vshaxe", "json-rpc");
		haxelibInstallGit("Simn", "haxeserver");

		changeDirectory(displayDir);
		runCommand("haxe", ["build.hxml"]);
	}
}