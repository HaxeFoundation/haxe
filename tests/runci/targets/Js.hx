package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;
import haxe.io.Path;
import sys.io.Process;

using StringTools;

class Js {
	static public function getJSDependencies() {
		switch [ci, systemName] {
			case [_, "Linux"]:
				if (commandSucceed("node", ["-v"])) {
					infoMsg('node has already been installed.');
				} else {
					Linux.requireAptPackages(["nodejs"]);
				}
			case [AzurePipelines, "Mac"]:
				runCommand("brew", ["install", "node@10"], true);
				runCommand("brew", ["link", "--overwrite", "--force", "node@10"]);
			case _:
				//pass
		}

		runCommand("node", ["-v"]);
	}

	static public function run(args:Array<String>) {
		getJSDependencies();
		
		runci.targets.Java.getJavaDependencies(); // this is awkward
		haxelibInstallGit("Simn", "haxeserver");
		changeDirectory(serverDir);
		runCommand("haxe", ["build.hxml"]);
		runCommand("node", ["test.js"]);
	}
}