package runci.targets;

import runci.Config.*;
import runci.System.*;

class Php {
	static final miscPhpDir = getMiscSubDir('php');

	static public function getPhpDependencies() {
		final phpCmd = commandResult("php", ["-v"]);
		final phpVerReg = ~/PHP ([0-9]+\.[0-9]+)/i;
		final phpVer = if (phpVerReg.match(phpCmd.stdout)) Std.parseFloat(phpVerReg.matched(1)); else null;

		if (phpCmd.exitCode == 0 && phpVer != null && phpVer >= 7.0) {
			switch systemName {
				case "Linux":
					var phpInfo = commandResult("php", ["-i"]);
					if (phpInfo.stdout.indexOf("mbstring => enabled") < 0) {
						Linux.requireAptPackages(["php-mbstring"]);
					}
				case _:
			}
			infoMsg('php $phpVer has already been installed.');
			return;
		}
		switch systemName {
			case "Linux":
				// TODO: install php-sqlite3?
				Linux.requireAptPackages(["php-cli", "php-mbstring"]);
			case "Mac":
				runNetworkCommand("brew", ["install", "php"]);
			case "Windows":
				runNetworkCommand("cinst", ["php", "-version", "7.1.8", "-y"]);
			case _:
				throw 'unknown system: $systemName';
		}
		runCommand("php", ["-v"]);
	}

	static public function run(args:Array<String>) {
		getPhpDependencies();

		changeDirectory(miscPhpDir);
		runCommand("haxe", ["run.hxml"]);

		final binDir = "bin/php";

		final prefixes = [[]];
		if (isCi()) {
			prefixes.push(['-D', 'php-prefix=haxe']);
			prefixes.push(['-D', 'php-prefix=my.pack']);
		}

		for (prefix in prefixes) {
			changeDirectory(unitDir);
			if (isCi())
				deleteDirectoryRecursively(binDir);

			runCommand("haxe", ["compile-php.hxml"].concat(prefix).concat(args));
			runCommand("php", [binDir + "/index.php"]);

			changeDirectory(sysDir);
			if (isCi())
				deleteDirectoryRecursively(binDir);

			runCommand("haxe", ["compile-php.hxml"].concat(prefix).concat(args));
			runThroughPhpVersions(runSysTest.bind(_, ["bin/php/Main/index.php"]));

			changeDirectory(asysDir);
			if (isCi()) {
				deleteDirectoryRecursively(binDir);
			}
			runCommand("haxe", ["compile-php.hxml"].concat(prefix).concat(args));
			runThroughPhpVersions(runCommand.bind(_, ['$binDir/index.php']));
		}
	}

	static function runThroughPhpVersions(fn:(phpCmd:String) -> Void) {
		switch [ci, systemName] {
			case [GithubActions, "Linux"]:
				for (version in ['7.4', '8.0']) {
					fn('php$version');
				}
			case _:
				fn('php');
		}
	}
}
