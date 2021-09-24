package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;

using haxe.io.Path;

class Php {
	static var miscPhpDir(get,never):String;
	static inline function get_miscPhpDir() return miscDir + 'php/';

	static final windowsPhpIni = cwd + 'PHP.ini';

	static var windowsPhpExtPath(get, null) = null;
	static function get_windowsPhpExtPath() {
		if (windowsPhpExtPath != null)
			return windowsPhpExtPath;

		final phpPath = commandResult("where", ["php"]).stdout;
		return windowsPhpExtPath = Path.join([phpPath.directory(), "ext"]);
	}


	static function generateArgs(file:String) {
		if (systemName != "Windows")
			return [file];
		return [
			"-c",
			windowsPhpIni,
			"-d",
			'extension_dir=$windowsPhpExtPath',
			file
		];
	}

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
					var phpInfo = commandResult("php", ["-i"]).stdout;
					if(phpInfo.indexOf("mbstring => enabled") < 0) {
						Linux.requireAptPackages(["php-mbstring"]);
					}
				case _:
			}
			infoMsg('php $phpVer has already been installed.');
			return;
		}
		switch systemName {
			case "Linux":
				Linux.requireAptPackages(["php-cli", "php-mbstring", "php-sqlite3"]);
			case "Mac":
				runCommand("brew", ["install", "php"], true);
			case "Windows":
				runCommand("cinst", ["php", "-version", "7.1.8", "-y"], true);
			case _:
				throw 'unknown system: $systemName';
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
			if(isCi())
				deleteDirectoryRecursively(binDir);

			runCommand("haxe", ["compile-php.hxml"].concat(prefix).concat(args));
			runCommand("php", generateArgs(binDir + "/index.php"));

			changeDirectory(sysDir);
			if(isCi())
				deleteDirectoryRecursively(binDir);

			runCommand("haxe", ["compile-php.hxml"].concat(prefix).concat(args));
			runSysTest("php", generateArgs(binDir + "/Main/index.php"));
		}
	}
}
