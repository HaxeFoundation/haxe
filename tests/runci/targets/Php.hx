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
			switch systemName {
				case "Linux":
					var phpInfo = commandResult("php", ["-i"]);
					if(phpInfo.stdout.indexOf("mbstring => enabled") < 0) {
						Linux.requireAptPackages(["php-mbstring"]);
					}
				case _:
			}
			infoMsg('php ${phpVer} has already been installed.');
			return;
		}
		switch [ci, systemName] {
			case [TravisCI, "Linux"]:
				runCommand("phpenv", ["global", "7.0"], false, true);
			case [_, "Linux"]:
				Linux.requireAptPackages(["php-cli", "php-mbstring"]);
			case [_, "Mac"]:
				runCommand("brew", ["install", "php"], true);
			case [_, "Windows"]:
				runCommand("cinst", ["php", "-version", "7.1.8", "-y"], true);
			case _:
				throw 'unknown combination: $ci, $systemName';
		}
		runCommand("php", ["-v"]);
	}

	static public function run(args:Array<String>) {
		getPhpDependencies();

		changeDirectory(miscPhpDir);
		runCommand("haxe", ["run.hxml"]);

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
			runThroughPhpVersions(runCommand.bind(_, [binDir + "/index.php"]));

			changeDirectory(sysDir);
			if(isCi()) {
				deleteDirectoryRecursively(binDir);
			}
			runCommand("haxe", ["compile-php.hxml"].concat(prefix).concat(args));
			runThroughPhpVersions(runCommand.bind(_, ["bin/php/Main/index.php"]));
		}
	}

	static function runThroughPhpVersions(fn:(phpCmd:String)->Void) {
		switch [ci, systemName] {
			case [TravisCI, "Linux"]:
				for(version in ['7.0', '7.1'/*, '7.2', '7.3'*/]) { //7.2 and 7.3 are not available on travis Ubuntu trusty
					runCommand("phpenv", ["global", version]);
					fn('php');
				}
			case [GithubActions, "Linux"]:
				for(version in ['7.1', '7.2', '7.3', '7.4']) {
					fn('php$version');
				}
			case _:
				fn('php');
		}
	}
}