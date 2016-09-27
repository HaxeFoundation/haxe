package io;

import sys.io.Process;

class TestProcess extends TestCommandBase {
	override function run(cmd:String, ?args:Array<String>):Int {
		var p = new Process(cmd, args);
		var exitCode = p.exitCode();
		runInfo = {
			out: p.stdout.readAll().toString(),
			err: p.stderr.readAll().toString(),
		};
		p.close();
		return exitCode;
	}
}
