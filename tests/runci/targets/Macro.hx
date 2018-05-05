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
		runCommand("haxe", ["build.hxml"]);

		changeDirectory(sourcemapsDir);
		runCommand("haxe", ["run.hxml"]);

		changeDirectory(miscDir);
		getCsDependencies();
		getPythonDependencies();
		runCommand("haxe", ["compile.hxml"]);

		changeDirectory(sysDir);
		haxelibInstall("utest");
		runCommand("haxe", ["compile-macro.hxml"]);
		runCommand("haxe", ["compile-each.hxml", "--run", "Main"]);
	}
}