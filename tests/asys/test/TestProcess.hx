package test;

import haxe.io.Bytes;
import utest.Async;

class TestProcess extends Test {
	function testPipes(async:Async) {
		if (Sys.systemName() == "Windows") { // TODO
			t(true);
			async.done();
			return;
		}

		var proc = asys.Process.spawn("cat");
		proc.stdout.dataSignal.on(data -> {
			beq(data, TestConstants.helloBytes);
			proc.kill();
			proc.close((err) -> {
				eq(err, null);
				async.done();
			});
		});
		proc.stdin.write(TestConstants.helloBytes);

		TestBase.uvRun();
	}
}
