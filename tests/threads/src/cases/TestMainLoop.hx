package cases;

import haxe.MainLoop;

@:timeout(10000)
@:depends(cases.TestEvents)
class TestMainLoop extends utest.Test {
	@:keep static var init = MainLoop.add(() -> {
		staticInit = true;
		init.stop();
	});
	static var staticInit = false;

	// function testWorksInStaticInits_issue10114(async:Async) {
	// 	var mainThread = Thread.current();
	// 	var checkAttempts = 3;
	// 	function check() {
	// 		checkAttempts++;
	// 		if(staticInit) {
	// 			pass();
	// 			async.done();
	// 		} else if(checkAttempts > 0) {
	// 			checkAttempts--;
	// 			mainThread.events.run(check);
	// 		} else {
	// 			fail();
	// 			async.done();
	// 		}
	// 	}
	// 	mainThread.events.run(check);
	// }

	// function testNewAction_immediately(async:Async) {
	// 	var e1:MainEvent = null;
	// 	e1 = MainLoop.add(() -> {
	// 		e1.stop();
	// 		var e2:MainEvent = null;
	// 		e2 = MainLoop.add(() -> {
	// 			e2.stop();
	// 			pass();
	// 			async.done();
	// 		});
	// 		e2.delay(0);
	// 	});
	// 	e1.delay(0);
	// }
}