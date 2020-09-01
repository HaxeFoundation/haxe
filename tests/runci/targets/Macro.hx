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

		changeDirectory(miscDir + "resolution");
		runCommand("haxe", ["run.hxml"]);

		changeDirectory(sysDir);
		runCommand("haxe", ["compile-macro.hxml"].concat(args));

		switch Sys.systemName() {
			case 'Linux':
				changeDirectory(miscDir + 'compiler_loops');
				runCommand("haxe", ["run.hxml"]);
			case _: // TODO
		}

		changeDirectory(eventLoopDir);
		runCommand("haxe", ["build.hxml"].concat(args).concat(["--interp"]));

		// changeDirectory(threadsDir);
		// runCommand("haxe", ["build.hxml", "--interp"]);
	}
}