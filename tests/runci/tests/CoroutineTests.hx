package runci.tests;

import runci.System.*;
import runci.Config.*;

class CoroutineTests {
	static public function run(baseArgs:Array<String>, ?afterwards:(args:Array<String>) -> Void) {
		infoMsg("Test coroutines:");
		changeDirectory(getMiscSubDir("coroutines"));
		for (opt in [[], ["-D", "coroutine.noopt"]]) {
			for (thro in [[], ["-D", "coroutine.throw"]]) {
				var args = baseArgs.concat(opt).concat(thro);
				infoMsg("Running " + args.join(" "));
				runCommand("haxe", args);
				if (afterwards != null) {
					afterwards(args);
				}
			}
		}
	}
}