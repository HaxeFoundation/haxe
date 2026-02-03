package cases;

import haxe.EventLoop;

@:timeout(2000)
class TestEvents extends utest.Test {

	function testIssue10567_runEventsInOrderByTime(async:Async) {
		var events = EventLoop.current;
		var checks = [];
		var e3 = null;
		var e2 = null;
		var e1 = null;
		e2 = events.addTimer(() -> {
			checks.push(2);
			e1.stop();
			e2.stop();
			e3.stop();
		}, 20 / 1000);
		e1 = events.addTimer(() -> checks.push(1), 10 / 1000);
		e3 = events.addTimer(() -> checks.push(3), 30 / 1000);
		Sys.sleep(0.1);

		var checker = null;
		checker = events.addTimer(() -> {
			same([1, 2], checks);
			async.done();
			checker.stop();
		}, 100 / 1000);
	}

	function testRun(async:Async) {
		var mainThread = Thread.current();
		Thread.create(() -> {
			var childThread = Thread.current();
			isTrue(mainThread != childThread);
			EventLoop.getThreadLoop(mainThread).run(() -> {
				isTrue(mainThread == Thread.current());
				EventLoop.getThreadLoop(childThread).run(() -> {
					isTrue(childThread == Thread.current());
					EventLoop.getThreadLoop(mainThread).run(() -> {
						isTrue(mainThread == Thread.current());
						async.done();
					});
				});
			});
			//keep child thread alive while main thread is adding an event to run in it
			Sys.sleep(0.5);
		});
	}

	@:depends(testRun)
	function testRepeat(async:Async) {
		function test(thread:Thread, done:()->Void) {
			var timesExecuted = 0;
			var eventHandler = null;
			eventHandler = EventLoop.getThreadLoop(thread).addTimer(() -> {
				++timesExecuted;
				isTrue(thread == Thread.current());
				if(timesExecuted >= 3) {
					eventHandler.stop();
					done();
				}
			}, 50 / 1000);
		}

		var mainThread = Thread.current();
		//test in main thread
		test(mainThread, () -> {
			//now test in a child thread
			Thread.create(() -> {
				var childThread = Thread.current();
				isTrue(childThread != mainThread);
				test(childThread, EventLoop.getThreadLoop(mainThread).run.bind(() -> async.done(),0));
			});
		});
	}

}