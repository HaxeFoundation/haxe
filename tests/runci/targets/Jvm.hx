package runci.targets;

import runci.System.*;
import runci.Config.*;

class Jvm {
	static public function getJavaDependencies() {
		haxelibInstallGit("HaxeFoundation", "hxjava", true);
		haxelibInstallGit("HaxeFoundation", "format", "jvm", "--always");
		runCommand("javac", ["-version"]);
	}

	static final miscJavaDir = getMiscSubDir('java');

	static public function run(args:Array<String>) {
		deleteDirectoryRecursively("bin/jvm");
		getJavaDependencies();

		runCommand("haxe", ["compile-java-native.hxml"]);

		for (level in 0...3) {
			final args = args.concat(["-D", "jvm.dynamic-level=" + level]);
			runCommand("haxe", ["compile-jvm-only.hxml", "--hxb", "bin/hxb/jvm.zip"].concat(args));
			runCommand("java", ["-jar", "bin/unit.jar"]);

			runCommand("haxe", ["compile-jvm-only.hxml", "--hxb-lib", "bin/hxb/jvm.zip"].concat(args));
			runCommand("java", ["-jar", "bin/unit.jar"]);

			runCommand("haxe", ["compile-jvm-only.hxml","-dce","no"].concat(args));
			runCommand("java", ["-jar", "bin/unit.jar"]);

			runCommand("haxe", ["compile-jvm-only.hxml", "--hxb-lib", "bin/hxb/jvm.zip"].concat(args));
			runCommand("java", ["-jar", "bin/unit.jar"]);
		}

		infoMsg("Test coroutines:");
		changeDirectory(getMiscSubDir("coroutines"));
		runCommand("haxe", ["build-jvm.hxml"]);

		changeDirectory(miscJavaDir);
		runCommand("haxe", ["run.hxml"]);

		changeDirectory(sysDir);
		runCommand("haxe", ["compile-jvm.hxml"].concat(args));
		runSysTest("java", ["-jar", "bin/jvm/sys.jar"]);

		changeDirectory(threadsDir);
		runCommand("haxe", ["build.hxml", "--jvm", "export/threads.jar"].concat(args));
		runCommand("java", ["-jar", "export/threads.jar"]);
	}
}
