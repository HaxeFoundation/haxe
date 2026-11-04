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

	#if eval
	function testIssue8210_nonExistentCommand() {
		var exitCode = run("haxe_issue_8210_does_not_exist", []);
		utest.Assert.equals(127, exitCode);
		utest.Assert.isTrue(runInfo.err.indexOf("haxe_issue_8210_does_not_exist") >= 0);
	}
	#end
}
