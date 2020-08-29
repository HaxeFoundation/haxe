package cases;

@:timeout(1000)
class TestEvents extends utest.Test {

	function testRunEvent(async:Async) {
		var mainThread = Thread.current();
		Thread.create(() -> {
			var childThread = Thread.current();
			isTrue(mainThread != childThread);
			mainThread.runEvent(() -> {
				isTrue(mainThread == Thread.current());
				childThread.runEvent(() -> {
					isTrue(childThread == Thread.current());
					mainThread.runEvent(() -> {
						isTrue(mainThread == Thread.current());
						async.done();
					});
				});
			});
			//keep child thread alive while main thread is adding an event to run in it
			Sys.sleep(0.5);
		});
	}

	@:depends(testRunEvent)
	function testRepeatEvent(async:Async) {
		function test(thread:Thread, done:()->Void) {
			var timesExecuted = 0;
			var eventHandler = null;
			eventHandler = Thread.repeatEvent(() -> {
				++timesExecuted;
				isTrue(thread == Thread.current());
				if(timesExecuted >= 3) {
					Thread.cancelEvent(eventHandler);
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
				test(childThread, mainThread.runEvent.bind(() -> async.done()));
			});
		});
	}

	@:depends(testRunEvent)
	function testPromisedEvents(async:Async) {
		var mainThread = Thread.current();
		// this thread is expected to wait for promised events
		Thread.create(() -> {
			var eventsExecuted = 0;
			var testThread = Thread.current();
			testThread.promiseEvent(); // 1 promised event
			// this thread will deliver promised events to the testThread
			Thread.create(() -> {
				Sys.sleep(0.2);
				testThread.promiseEvent(); // 2 promised events
				testThread.runPromisedEvent(() -> {
					++eventsExecuted;
					isTrue(testThread == Thread.current());
				});
				testThread.promiseEvent(); // 3 promised events
				Sys.sleep(0.2);
				testThread.runPromisedEvent(() -> {
					++eventsExecuted;
					isTrue(testThread == Thread.current());
				});
				Sys.sleep(0.2);
				testThread.runPromisedEvent(() -> {
					++eventsExecuted;
					isTrue(testThread == Thread.current());
					mainThread.runEvent(() -> {
						equals(3, eventsExecuted);
						async.done();
					});
				});
			});
		});
	}
}