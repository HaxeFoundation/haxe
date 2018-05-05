package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;

class Cs {
	static public function getCsDependencies() {
		switch (systemName) {
			case "Linux":
				if (commandSucceed("mono", ["--version"]))
					infoMsg('mono has already been installed.');
				else
					Linux.requireAptPackages(["mono-devel", "mono-mcs"]);
				runCommand("mono", ["--version"]);
			case "Mac":
				if (commandSucceed("mono", ["--version"]))
					infoMsg('mono has already been installed.');
				else
					runCommand("brew", ["install", "mono"], true);
				runCommand("mono", ["--version"]);
			case "Windows":
				switch (ci) {
					case AppVeyor:
						addToPATH("C:\\Program Files (x86)\\Mono\\bin");
						runCommand("mono", ["--version"]);
					case _:
						//pass
				}
		}

		haxelibInstallGit("HaxeFoundation", "hxcs", true);
	}

	static public function runCs(exe:String, ?args:Array<String>):Void {
		if (args == null) args = [];
		exe = FileSystem.fullPath(exe);
		switch (systemName) {
			case "Linux", "Mac":
				runCommand("mono", [exe].concat(args));
			case "Windows":
				runCommand(exe, args);
		}
	}

	static public function run(args:Array<String>) {
		getCsDependencies();

		var compl = switch [ci, systemName] {
			case [TravisCI, "Linux"]:
				"-travis";
			case _:
				"";
		};

		for (fastcast in      [[], ["-D", "fast_cast"]])
		for (noroot in        [[], ["-D", "no_root"]])
		for (erasegenerics in [[], ["-D", "erase_generics"]])
		{
			var extras = fastcast.concat(erasegenerics).concat(noroot);
			runCommand("haxe", ['compile-cs$compl.hxml'].concat(extras));
			runCs("bin/cs/bin/TestMain-Debug.exe");

			runCommand("haxe", ['compile-cs-unsafe$compl.hxml'].concat(extras));
			runCs("bin/cs_unsafe/bin/TestMain-Debug.exe");
		}

		runCommand("haxe", ['compile-cs$compl.hxml','-dce','no']);
		runCs("bin/cs/bin/TestMain-Debug.exe");

		changeDirectory(sysDir);
		haxelibInstall("utest");
		runCommand("haxe", ["compile-cs.hxml",'-D','fast_cast']);
		runCs("bin/cs/bin/Main-Debug.exe", []);

		changeDirectory(miscDir + "csTwoLibs");
		for (i in 1...5)
		{
			runCommand("haxe", ['compile-$i.hxml','-D','fast_cast']);
			runCs("bin/main/bin/Main.exe");
		}
	}
}