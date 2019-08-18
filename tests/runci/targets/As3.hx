package runci.targets;

import runci.System.*;

class As3 {
	static public function run(args:Array<String>) {
		runci.targets.Flash.setupFlashPlayerDebugger();
		runci.targets.Flash.setupFlexSdk();

		runCommand("haxe", ["compile-as3.hxml", "-D", "fdb"].concat(args));
		var success = runci.targets.Flash.runFlash("bin/unit9_as3.swf");
		if (!success)
			fail();
	}
}