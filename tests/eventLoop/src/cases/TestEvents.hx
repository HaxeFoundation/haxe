package cases;

@:timeout(1000)
class TestEvents extends utest.Test {

	function testThreadRunWithEventLoop() {
		var eventExecuted = false;
		var lock = new sys.thread.Lock();
		Thread.create(() -> {
			var thread = Thread.current();
			raises(
				() -> thread.events.run(() -> {}),
				sys.thread.NoEventLoopException
			);
			Thread.runWithEventLoop(() -> {
				thread.events.run(lock.release);
			});
		});
		lock.wait();
		pass();
	}

	function testRun(async:Async) {
		var mainThread = Thread.current();
		Thread.createWithEventLoop(() -> {
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
			Thread.createWithEventLoop(() -> {
				var childThread = Thread.current();
				isTrue(childThread != mainThread);
				test(childThread, mainThread.events.run.bind(() -> async.done()));
			});
		});
	}

	@:depends(testRun)
	function testPromisedEvents(async:Async) {
		var mainThread = Thread.current();
		// this thread is expected to wait for promised events
		Thread.createWithEventLoop(() -> {
			var eventsExecuted = 0;
			var testThread = Thread.current();
			testThread.events.promise(); // 1 promised event
			// this thread will deliver promised events to the testThread
			Thread.createWithEventLoop(() -> {
				Sys.sleep(0.2);
				testThread.events.promise(); // 2 promised events
				testThread.events.runPromised(() -> {
					++eventsExecuted;
					isTrue(testThread == Thread.current());
				});
				testThread.events.promise(); // 3 promised events
				Sys.sleep(0.2);
				testThread.events.runPromised(() -> {
					++eventsExecuted;
					isTrue(testThread == Thread.current());
				});
				Sys.sleep(0.2);
				testThread.events.runPromised(() -> {
					++eventsExecuted;
					isTrue(testThread == Thread.current());
					mainThread.events.run(() -> {
						equals(3, eventsExecuted);
						async.done();
					});
				});
			});
		});
	}
}