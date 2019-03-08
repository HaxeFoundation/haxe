package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;
import haxe.io.Path;
import sys.io.Process;

using StringTools;

class Js {
	static public function getJSDependencies() {
		switch (systemName) {
			case "Linux":
				if (commandSucceed("node", ["-v"])) {
					infoMsg('node has already been installed.');
				} else {
					Linux.requireAptPackages(["nodejs"]);
				}
			case "Mac":
				//pass
		}

		runCommand("node", ["-v"]);
	}

	static public function run(args:Array<String>) {
		getJSDependencies();

		runci.targets.Java.getJavaDependencies(); // this is awkward
		changeDirectory(serverDir);

        haxelibInstall("hxnodejs");
		haxelibInstallGit("vshaxe", "json-rpc");
		haxelibInstallGit("Simn", "haxeserver");

        runCommand("haxe", ["build.hxml"]);
        runCommand("node", ["test.js"]);
	}
}