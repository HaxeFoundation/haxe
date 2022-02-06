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
	
	static public function requireAptPackages(packages:Array<String>):Void {
		var notYetInstalled = [for (p in packages) if (!isAptPackageInstalled(p)) p];
		if (notYetInstalled.length > 0) {
			var aptCacheDir = Sys.getEnv("APT_CACHE_DIR");
			var baseCommand = if (aptCacheDir != null) {
				["apt-get", "-o", 'dir::cache::archives=${aptCacheDir}', "install", "-qqy"];
			} else {
				["apt-get", "install", "-qqy"];
			};
			runCommand("sudo", baseCommand.concat(notYetInstalled), true);
		}
	}
}

enum abstract Arch(Int) {
	final Arm64;
	final Amd64;
}