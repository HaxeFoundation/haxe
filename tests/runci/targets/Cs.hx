package runci.targets;

import runci.Config.*;
import runci.System.*;

class Cs {
	static final miscCsDir = getMiscSubDir('cs');

	static public function run(args:Array<String>) {
		deleteDirectoryRecursively("bin/cs");

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
