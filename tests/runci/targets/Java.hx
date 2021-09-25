package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;
using StringTools;

class Java {
	static var miscJavaDir(get,never):String;
	static inline function get_miscJavaDir() return miscDir + 'java/';

	static public function getJavaDependencies() {
		haxelibInstallGit("HaxeFoundation", "hxjava", true);
		runCommand("javac", ["-version"]);
	}

	static public function run(args:Array<String>) {
		deleteDirectoryRecursively("bin/java");
		getJavaDependencies();

		runCommand("haxe", ["compile-java.hxml"].concat(args));
		runCommand("java", ["-jar", "bin/java/TestMain-Debug.jar"]);

		runCommand("haxe", ["compile-java.hxml","-dce","no"].concat(args));
		runCommand("java", ["-jar", "bin/java/TestMain-Debug.jar"]);

		changeDirectory(miscJavaDir);
		runCommand("haxe", ["run.hxml"]);

		changeDirectory(sysDir);
		runCommand("haxe", ["compile-java.hxml"].concat(args));
		runSysTest("java", ["-jar", "bin/java/Main-Debug.jar"]);

		changeDirectory(threadsDir);
		runCommand("haxe", ["build.hxml", "-java", "export/java"].concat(args));
		runCommand("java", ["-jar", "export/java/Main.jar"]);

		infoMsg("Testing java-lib extras");
		changeDirectory('$unitDir/bin');
		final libTestDir = 'java-lib-tests';
		if (!FileSystem.exists(libTestDir))
			runNetworkCommand("git", ["clone", "https://github.com/waneck/java-lib-tests.git", "--depth", "1"]);

		for (dir in FileSystem.readDirectory(libTestDir)) {
			var path = '$libTestDir/$dir';
			if (FileSystem.isDirectory(path))
				for (file in FileSystem.readDirectory(path))
					if (file.endsWith('.hxml'))
						runCommand("haxe", ["--cwd", path, file]);
		}
	}
}
