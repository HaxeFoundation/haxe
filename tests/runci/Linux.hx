package runci;

import runci.System.*;

using StringTools;

class Linux {
	static public function isAptPackageInstalled(aptPackage:String):Bool {
		return commandSucceed("dpkg-query", ["-W", "-f='${Status}'", aptPackage]);
	}

	static inline function hasAptGet() {
		// CI always runs on ubuntu, otherwise check for apt-get
		return Config.isCi() || commandSucceed("type", ["apt-get"]);
	}

	static public function requireAptPackages(packages:Array<String>):Void {
		if (!hasAptGet()){
			infoMsg("System does not have apt-get installed.");
			return;
		}
		var notYetInstalled = [for (p in packages) if (!isAptPackageInstalled(p)) p];
		if (notYetInstalled.length > 0) {
			var aptCacheDir = Sys.getEnv("APT_CACHE_DIR");
			var baseCommand = if (aptCacheDir != null) {
				["apt-get", "-o", 'dir::cache::archives=${aptCacheDir}', "install", "-qqy"];
			} else {
				["apt-get", "install", "-qqy"];
			};
			runNetworkCommand("sudo", baseCommand.concat(notYetInstalled));
		}
	}
}
