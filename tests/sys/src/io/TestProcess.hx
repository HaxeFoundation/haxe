package io;

import sys.io.Process;

class TestProcess extends TestCommandBase {
	override function run(cmd:String, ?args:Array<String>):Int {
		var p = new Process(cmd, args);
		var exitCode = p.exitCode();
		p.close();
		return exitCode;
	}
}
