package runci.targets;

import runci.System.*;
import runci.Config.*;

class Macro {
	static public function run(args:Array<String>) {
		runCommand("haxe", ["compile-macro.hxml", "--hxb", "bin/hxb/eval.zip"].concat(args));
		runCommand("haxe", ["compile-macro.hxml", "--hxb-lib", "bin/hxb/eval.zip"].concat(args));

		haxelibInstallGit("Simn", "haxeserver");

		changeDirectory(sourcemapsDir);
		runCommand("haxe", ["run.hxml"]);

		changeDirectory(nullSafetyDir);
		infoMsg("No-target null safety:");
		runCommand("haxe", ["test.hxml"]);
		infoMsg("Js-es6 null safety:");
		runCommand("haxe", ["test-js-es6.hxml"]);

		changeDirectory(getMiscSubDir());
		runCommand("haxe", ["compile.hxml"]);

		changeDirectory(getMiscSubDir());
		runCommand("haxe", ["run-base.hxml", "--run", "Main", "eval/resolution"]);

		changeDirectory(sysDir);
		runCommand("haxe", args.concat(["compile-eval-hxb.hxml"]));
		runSysTest("haxe", ["--hxb-lib", "bin/eval/sys.hxb", "--run", "Main"]);

		switch Sys.systemName() {
			case 'Linux':
				changeDirectory(getMiscSubDir());
				runCommand("haxe", ["run-base.hxml", "-D", "timeout=3", "--run", "Main", "eval/compiler_loops"]);
			case _: // TODO
		}

		changeDirectory(getMiscSubDir("eval", "connect_stdin"));
		runCommand("haxe", ["run.hxml"]);

		changeDirectory(threadsDir);
		runCommand("haxe", ["build.hxml", "--interp"]);

		deleteDirectoryRecursively(partyDir);
		runCommand("mkdir", [partyDir]);
		party();
	}

	static function party() {
		changeDirectory(partyDir);
		runCommand("git", ["clone", "--depth=1", "https://github.com/haxetink/tink_core", "tink_core"]);
		changeDirectory("tink_core");
		runCommand("haxelib", ["newrepo"]);
		runCommand("haxelib", ["install", "tests.hxml", "--always"]);
		runCommand("haxelib", ["dev", "tink_core", "."]);
		runCommand("haxe", ["tests.hxml", "-w", "-WDeprecated", "--interp", "--macro", "addMetadata('@:exclude','Futures','testDelay')"]);

		changeDirectory(partyDir);
		runCommand("git", ["clone", "--depth=1", "-b", Config.hxcoroVersion, "https://github.com/HaxeFoundation/hxcoro", "hxcoro"]);
		changeDirectory("hxcoro");
		runCommand("haxelib", ["newrepo"]);
		runCommand("haxelib", ["git", "utest", "https://github.com/haxe-utest/utest.git"]);
		runCommand("haxelib", ["dev", "hxcoro", "."]);
		runCommand("haxe", ["--cwd", "tests", "build-eval.hxml"]);
		runCommand("haxe", ["--cwd", "tests", "build-eval.hxml", "--hxb", "bin/test.hxb"]);
		runCommand("haxe", ["--cwd", "tests", "build-eval.hxml", "--hxb-lib", "bin/test.hxb"]);
	}
}
