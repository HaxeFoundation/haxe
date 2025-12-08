package cases;

@:timeout(2000)
class TestEvents extends utest.Test {

	function testIssue10567_runEventsInOrderByTime(async:Async) {
		var events = Thread.current().events;
		var checks = [];
		var e3 = null;
		var e2 = null;
		var e1 = null;
		e2 = events.repeat(() -> {
			checks.push(2);
			events.cancel(e1);
			events.cancel(e2);
			events.cancel(e3);
		}, 20);
		e1 = events.repeat(() -> checks.push(1), 10);
		e3 = events.repeat(() -> checks.push(3), 30);
		Sys.sleep(0.1);

		var checker = null;
		checker = events.repeat(() -> {
			same([1, 2], checks);
			async.done();
			events.cancel(checker);
		}, 100);
	}

	function testRun(async:Async) {
		var mainThread = Thread.current();
		Thread.create(() -> {
			var childThread = Thread.current();
			isTrue(mainThread != childThread);
			mainThread.events.run(() -> {
				isTrue(mainThread == Thread.current());
				childThread.events.run(() -> {
					isTrue(childThread == Thread.current());
					mainThread.events.run(() -> {
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
			eventHandler = thread.events.repeat(() -> {
				++timesExecuted;
				isTrue(thread == Thread.current());
				if(timesExecuted >= 3) {
					thread.events.cancel(eventHandler);
					done();
				}
			}, 50);
		}

		var mainThread = Thread.current();
		//test in main thread
		test(mainThread, () -> {
			//now test in a child thread
			Thread.create(() -> {
				var childThread = Thread.current();
				isTrue(childThread != mainThread);
				test(childThread, mainThread.events.run.bind(() -> async.done(),0));
			});
		});
	}

}