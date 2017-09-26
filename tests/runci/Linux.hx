package runci;

import runci.System.*;

class Linux {
	static public function isAptPackageInstalled(aptPackage:String):Bool {
		return commandSucceed("dpkg-query", ["-W", "-f='${Status}'", aptPackage]);
	}

	static public function requireAptPackages(packages:Array<String>):Void {
		var notYetInstalled = [for (p in packages) if (!isAptPackageInstalled(p)) p];
		if (notYetInstalled.length > 0) {
			var aptCacheDir = Sys.getEnv("APT_CACHE_DIR");
			var baseCommand = if (aptCacheDir != null) {
				["apt-get", "-o", 'dir::cache::archives=${aptCacheDir}', "install", "-y"];
			} else {
				["apt-get", "install", "-y"];
			};
			runCommand("sudo", baseCommand.concat(notYetInstalled), true);
		}
	}
}