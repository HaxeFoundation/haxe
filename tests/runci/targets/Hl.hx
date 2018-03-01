package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;

class Hl {
	static public function run(args:Array<String>) {
		runCommand("haxe", ["compile-hl.hxml", "-D", "dump", "-D", "dump_ignore_var_ids"].concat(args));
		runCommand("hl", ["bin/unit.hl"]);
	}
}
