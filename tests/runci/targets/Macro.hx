package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;
import runci.targets.Cs.*;
import runci.targets.Python.*;

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
		getCsDependencies();
		getPythonDependencies();
		runCommand("haxe", ["compile.hxml"]);

		changeDirectory(sysDir);
		runCommand("haxe", ["compile-macro.hxml"]);
		runCommand("haxe", ["compile-each.hxml", "--run", "Main"]);

		// changeDirectory(threadsDir);
		// runCommand("haxe", ["build.hxml", "--interp"]);
	}
}