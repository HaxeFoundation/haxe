package runci.targets;

import runci.Config.*;
import runci.System.*;

class Cs {
	static final miscCsDir = getMiscSubDir('cs');

	static public function run(args:Array<String>) {
		deleteDirectoryRecursively("bin/cs");

		// Ensure we use the locally built haxe, not the system one.
		// This is critical because the new C# target is only available in the local build
		// if host computer is using an older haxe/haxelib expecing legacy C# target and hxcs lib.
		addToPATH(repoDir);
		Sys.putEnv("HAXE_STD_PATH", repoDir + "std");

		// Use local haxelib repository if it exists
		var haxelibPath = repoDir + ".haxelib";
		if (sys.FileSystem.exists(haxelibPath)) {
			Sys.putEnv("HAXELIB_PATH", haxelibPath);
		}

		runCommand("dotnet", ["--version"]);

		runCommand("haxe", ["compile-cs.hxml"].concat(args));
		changeDirectory("bin/cs");
		runCommand("dotnet", ["run"]);
		changeDirectory(unitDir);

		Display.maybeRunDisplayTests(Cs);

		// Misc tests
		changeDirectory(miscCsDir);
		runCommand("haxe", ["run.hxml"]);

		changeDirectory(sysDir);
		runCommand("haxe", ["compile-cs.hxml"].concat(args));
		runSysTest("dotnet", ["run", "--project", "bin/cs/Project.csproj"]);

		changeDirectory(threadsDir);
		runCommand("haxe", ["build.hxml", "-cs", "export/cs"].concat(args));
		changeDirectory("export/cs");
		runCommand("dotnet", ["run"]);
	}
}
