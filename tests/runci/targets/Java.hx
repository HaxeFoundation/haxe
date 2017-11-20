package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;
using StringTools;

class Java {
	static public function getJavaDependencies() {
		haxelibInstallGit("HaxeFoundation", "hxjava", true);
		runCommand("javac", ["-version"]);
	}

	static public function run(args:Array<String>) {
		getJavaDependencies();
		runCommand("haxe", ["compile-java.hxml"].concat(args));
		runCommand("java", ["-jar", "bin/java/TestMain-Debug.jar"]);

		runCommand("haxe", ["compile-java.hxml","-dce","no"].concat(args));
		runCommand("java", ["-jar", "bin/java/TestMain-Debug.jar"]);

		changeDirectory(sysDir);
		haxelibInstall("utest");
		runCommand("haxe", ["compile-java.hxml"]);
		runCommand("java", ["-jar", "bin/java/Main-Debug.jar"]);

		infoMsg("Testing java-lib extras");
		changeDirectory('$unitDir/bin');
		runCommand("git", ["clone", "https://github.com/waneck/java-lib-tests.git", "--depth", "1"], true);
		for (dir in FileSystem.readDirectory('java-lib-tests'))
		{
			var path = 'java-lib-tests/$dir';
			if (FileSystem.isDirectory(path)) for (file in FileSystem.readDirectory(path))
			{
				if (file.endsWith('.hxml'))
				{
					runCommand("haxe", ["--cwd",'java-lib-tests/$dir',file]);
				}
			}
		}
	}
}