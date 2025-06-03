package runci.tests;

import runci.System.*;
import runci.Config.*;

class CoroutineTests {
	static public function run(baseArgs:Array<String>, ?afterwards:(args:Array<String>) -> Void) {
		infoMsg("Test coroutines:");
		changeDirectory(getMiscSubDir("coroutines"));
		for (opt in [[], ["-D", "coroutine.noopt"]]) {
			var args = baseArgs.concat(opt);
			infoMsg("Running " + args.join(" "));
			runCommand("haxe", args);
			if (afterwards != null) {
				afterwards(args);
			}
		}
	}
}