package runci.targets;

import runci.System.*;
import runci.Config.*;

class Jvm {
	static public function run(args:Array<String>) {
		runCommand("haxe", ["compile-jvm.hxml"].concat(args));
		runCommand("java", ["-jar", "bin/unit.jar"]);

		runCommand("haxe", ["compile-jvm.hxml","-dce","no"].concat(args));
		runCommand("java", ["-jar", "bin/unit.jar"]);

		changeDirectory(sysDir);
		runCommand("haxe", ["compile-jvm.hxml"].concat(args));
		runCommand("java", ["-jar", "bin/jvm/sys.jar"]);

		changeDirectory(threadsDir);
		runCommand("haxe", ["build.hxml", "--jvm", "export/threads.jar"].concat(args));
		if (systemName != "Windows") { // #8154
			runCommand("java", ["-jar", "export/threads.jar"]);
		}
	}
}