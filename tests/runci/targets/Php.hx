package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;

class Php {
	static var miscPhpDir(get,never):String;
	static inline function get_miscPhpDir() return miscDir + 'php/';

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

		var binDir = "bin/php";

		var prefixes = [[]];
		if(isCi()) {
			prefixes.push(['-D', 'php-prefix=haxe']);
			prefixes.push(['-D', 'php-prefix=my.pack']);
		}

		for(prefix in prefixes) {
			changeDirectory(unitDir);
			if(isCi()) {
				deleteDirectoryRecursively(binDir);
			}

			runCommand("haxe", ["compile-php.hxml"].concat(prefix).concat(args));
			runThroughPhpVersions(runCommand.bind("php", [binDir + "/index.php"]));

			changeDirectory(sysDir);
			if(isCi()) {
				deleteDirectoryRecursively(binDir);
			}
			runCommand("haxe", ["compile-php.hxml"].concat(prefix));
			runThroughPhpVersions(runCommand.bind("php", ["bin/php/Main/index.php"]));

			changeDirectory(miscPhpDir);
			runThroughPhpVersions(runCommand.bind("haxe", ["run.hxml"]));
		}
	}

	static function runThroughPhpVersions(fn:()->Void) {
	if(isCi() && systemName == "Linux") {
			for(version in ['7.0', '7.1', '7.2', '7.3']) {
				runCommand("phpenv", ["global", version]);
				fn();
			}
		} else {
			fn();
		}
	}
}