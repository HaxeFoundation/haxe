package runci.targets;

import runci.System.*;
import runci.Config.*;

class Jvm {
	static public function run(args:Array<String>) {
		runCommand("haxe", ["compile-jvm.hxml"].concat(args));
		runCommand("java", ["-jar", "bin/jvm/TestMain-Debug.jar"]);

		runCommand("haxe", ["compile-jvm.hxml","-dce","no"].concat(args));
		runCommand("java", ["-jar", "bin/jvm/TestMain-Debug.jar"]);

		changeDirectory(sysDir);
		runCommand("haxe", ["compile-jvm.hxml"].concat(args));
		runCommand("java", ["-jar", "bin/jvm/Main-Debug.jar"]);

		changeDirectory(threadsDir);
		runCommand("haxe", ["build.hxml", "-java", "export/jvm", "-D", "jvm"].concat(args));
		if (systemName != "Windows") { // #8154
			runCommand("java", ["-jar", "export/jvm/Main.jar"]);
		}
	}
}