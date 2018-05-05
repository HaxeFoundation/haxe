package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;

class Php {
	static public function getPhpDependencies() {
		var phpCmd = commandResult("php", ["-v"]);
		var phpVerReg = ~/PHP ([0-9]+\.[0-9]+)/i;
		var phpVer = if (phpVerReg.match(phpCmd.stdout))
			Std.parseFloat(phpVerReg.matched(1));
		else
			null;

		if (phpCmd.exitCode == 0 && phpVer != null && phpVer >= 7.0) {
			infoMsg('php ${phpVer} has already been installed.');
			return;
		}
		switch (systemName) {
			case "Linux":
				runCommand("phpenv", ["global", "7.0"], false, true);
			case "Mac":
				runCommand("brew", ["install", "php"], true);
			case "Windows":
				runCommand("cinst", ["php", "-version", "7.1.8", "-y"], true);
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