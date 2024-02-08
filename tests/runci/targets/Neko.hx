package runci.targets;

import runci.System.*;
import runci.Config.*;

class Neko {
	static public function run(args:Array<String>) {
		runCommand("haxe", ["compile-neko.hxml", "-D", "dump", "-D", "dump_ignore_var_ids"].concat(args));
		runCommand("neko", ["bin/unit.n"]);

		changeDirectory(getMiscSubDir('neko'));
		runCommand("haxe", ["run.hxml"].concat(args));

		changeDirectory(sysDir);
		runCommand("haxe", ["compile-neko.hxml"].concat(args));
		runSysTest("neko", ["bin/neko/sys.n"]);

		changeDirectory(threadsDir);
		runCommand("haxe", ["build.hxml", "--neko", "export/threads.n"]);
		runCommand("neko", ["export/threads.n"]);
	}
}
