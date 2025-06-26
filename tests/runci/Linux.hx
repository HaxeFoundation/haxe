package runci;

import runci.System.*;

using StringTools;

class Linux {
	static public var arch(get, null):Arch;

	static function get_arch() {
		if(arch == null)
			arch = switch commandResult('arch', []).stdout.replace('\n', '') {
				case 'arm64' | 'aarch64': Arm64;
				case _: Amd64;
			}
		return arch;
	}

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

enum abstract Arch(Int) {
	final Arm64;
	final Amd64;
}
