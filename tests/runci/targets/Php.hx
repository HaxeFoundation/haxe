package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;

class Php {
	static public function getPhpDependencies() {
		switch (systemName) {
			case "Linux":
				var phpCmd = commandResult("php", ["-v"]);
				var phpVerReg = ~/PHP ([0-9]+\.[0-9]+)/i;
				var phpVer = if (phpVerReg.match(phpCmd.stdout))
					Std.parseFloat(phpVerReg.matched(1));
				else
					null;
				if (phpCmd.exitCode == 0 && phpVer != null && phpVer >= 5.5) {
					infoMsg('php has already been installed.');
				} else {
					Linux.requireAptPackages(["php5-cli"]);
				}
			case "Mac":
				//pass
			case "Windows":
				if (commandSucceed("php", ["-v"])) {
					infoMsg('php has already been installed.');
				} else {
					runCommand("cinst", ["php", "-version", "5.6.3", "-y"], true);
					addToPATH("C:\\tools\\php");
				}
		}
		runCommand("php", ["-v"]);
	}

	static public function run(args:Array<String>) {
		getPhpDependencies();
		runCommand("haxe", ["compile-php.hxml"].concat(args));
		runCommand("php", ["bin/php/index.php"]);

		changeDirectory(sysDir);
		haxelibInstall("utest");
		runCommand("haxe", ["compile-php.hxml"]);
		runCommand("php", ["bin/php/Main/index.php"]);
	}
}