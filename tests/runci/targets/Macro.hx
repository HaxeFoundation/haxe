package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;

class Macro {
	static public function run(args:Array<String>) {
		runCommand("haxe", ["compile-macro.hxml"].concat(args));

		changeDirectory(displayDir);
		haxelibInstallGit("Simn", "haxeserver");
		runCommand("haxe", ["build.hxml"]);

		changeDirectory(sourcemapsDir);
		runCommand("haxe", ["run.hxml"]);

		changeDirectory(nullSafetyDir);
		infoMsg("No-target null safety:");
		runCommand("haxe", ["test.hxml"]);
		infoMsg("Js-es6 null safety:");
		runCommand("haxe", ["test-js-es6.hxml"]);

		changeDirectory(miscDir);
		runCommand("haxe", ["compile.hxml"]);

		changeDirectory(sysDir);
		runCommand("haxe", ["compile-macro.hxml"]);

		// changeDirectory(threadsDir);
		// runCommand("haxe", ["build.hxml", "--interp"]);
	}
}