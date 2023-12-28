package runci.targets;

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

		changeDirectory(getMiscSubDir());
		runCommand("haxe", ["compile.hxml"]);

		changeDirectory(getMiscSubDir("resolution"));
		runCommand("haxe", ["run.hxml"]);

		changeDirectory(sysDir);
		runSysTest("haxe", ["compile-macro.hxml"].concat(args));

		switch Sys.systemName() {
			case 'Linux':
				changeDirectory(getMiscSubDir('compiler_loops'));
				runCommand("haxe", ["run.hxml"]);
			case _: // TODO
		}

		changeDirectory(threadsDir);
		runCommand("haxe", ["build.hxml", "--interp"]);

		deleteDirectoryRecursively(partyDir);
		runCommand("mkdir", [partyDir]);
		changeDirectory(partyDir);
		party();
	}

	static function party() {
		runCommand("git", ["clone", "https://github.com/haxetink/tink_core", "tink_core"]);
		changeDirectory("tink_core");
		runCommand("haxelib", ["newrepo"]);
		runCommand("haxelib", ["install", "tests.hxml", "--always"]);
		runCommand("haxelib", ["dev", "tink_core", "."]);
		runCommand("haxe", ["tests.hxml", "-w", "-WDeprecated", "--interp", "--macro", "addMetadata('@:exclude','Futures','testDelay')"]);
	}
}
