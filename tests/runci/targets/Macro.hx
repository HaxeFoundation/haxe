package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;
import runci.targets.Cs.*;
import runci.targets.Python.*;

class Macro {
	static public function run(args:Array<String>) {
		runCommand("haxe", ["compile-macro.hxml"].concat(args));

		haxelibInstall("utest");

		// TODO: enable this again at some point
		// changeDirectory(displayDir);
		// runCommand("haxe", ["build.hxml"]);

		changeDirectory(sourcemapsDir);
		runCommand("haxe", ["run.hxml"]);

		changeDirectory(nullSafetyDir);
		runCommand("haxe", ["test.hxml"]);

		changeDirectory(miscDir);
		getCsDependencies();
		getPythonDependencies();
		runCommand("haxe", ["compile.hxml"]);

		changeDirectory(sysDir);
		runCommand("haxe", ["compile-macro.hxml"]);
		runCommand("haxe", ["compile-each.hxml", "--run", "Main"]);
	}
}