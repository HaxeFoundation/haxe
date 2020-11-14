package cases;

import haxe.MainLoop;

@:timeout(10000)
@:depends(cases.TestEvents)
class TestMainLoop extends utest.Test {
	function testNewAction_immediately(async:Async) {
		var e1:MainEvent = null;
		e1 = MainLoop.add(() -> {
			e1.stop();
			var e2:MainEvent = null;
			e2 = MainLoop.add(() -> {
				e2.stop();
				pass();
				async.done();
			});
			e2.delay(0);
		});
		e1.delay(0);
	}
}