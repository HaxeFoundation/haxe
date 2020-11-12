package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;

class Cs {
	static var miscCsDir(get,never):String;
	static inline function get_miscCsDir() return miscDir + 'cs/';

	static public function getCsDependencies() {
		switch (systemName) {
			case "Linux":
				if (!isCi() && commandSucceed("mono", ["--version"]))
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
				//pass
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

		for (fastcast in      [[], ["-D", "fast_cast"]])
		for (noroot in        [[], ["-D", "no_root"]])
		for (erasegenerics in [[], ["-D", "erase_generics"]])
		{
			var extras = fastcast.concat(erasegenerics).concat(noroot);
			runCommand("haxe", ['compile-cs.hxml'].concat(extras).concat(args));
			runCs("bin/cs/bin/TestMain-Debug.exe");

			runCommand("haxe", ['compile-cs-unsafe.hxml'].concat(extras).concat(args));
			runCs("bin/cs_unsafe/bin/TestMain-Debug.exe");
		}

		runCommand("haxe", ['compile-cs.hxml','-dce','no'].concat(args));
		runCs("bin/cs/bin/TestMain-Debug.exe");

		changeDirectory(sysDir);
		runCommand("haxe", ["compile-cs.hxml",'-D','fast_cast'].concat(args));
		runCs("bin/cs/bin/Main-Debug.exe", []);

		changeDirectory(threadsDir);
		runCommand("haxe", ["build.hxml", "-cs", "export/cs"]);
		runCs("export/cs/bin/Main.exe");

		changeDirectory(eventLoopDir);
		runCommand("haxe", ["build.hxml", "--cs", "bin/cs"]);
		runCs("bin/cs/bin/Main.exe");

		changeDirectory(miscCsDir);
		runCommand("haxe", ["run.hxml"]);

		changeDirectory(miscCsDir + "csTwoLibs");
		for (i in 1...5)
		{
			runCommand("haxe", ['compile-$i.hxml','-D','fast_cast']);
			runCs("bin/main/bin/Main.exe");
		}
	}
}