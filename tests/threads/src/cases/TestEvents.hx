package cases;

import utest.Assert;
import haxe.EventLoop;

@:timeout(2000)
class TestEvents extends ThreadTestBase {

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

	function testBlocking() {
		var threadValue = null;
		EventLoop.addTask(() -> {
			Sys.sleep(0.1);
			threadValue = "ok";
		});

		while (EventLoop.hasRunningThreads()) {
			Sys.sleep(0.01);
		}

		Assert.equals("ok", threadValue);
	}

	function testBlockingInstance() {
		var threadValue = null;
		final loop = new EventLoop();
		loop.addThreadTask(() -> {
			Sys.sleep(0.1);
			threadValue = "ok";
		});

		loop.loop();

		Assert.equals("ok", threadValue);
	}

	#if hl
	/**
		Cross-thread EventLoop.run must wake a thread blocked in UV_RUN_ONCE
		via nativeLoop.wake() (uv_async_send), not by spinning on NoWait.
	**/
	@:timeout(3000)
	function testNativeWake(async:Async) {
		final mainLoop = EventLoop.main;
		final tcp = new hl.uv.Tcp(hl.uv.Loop.getFromEventLoop(mainLoop));
		tcp.bind(new sys.net.Host("127.0.0.1"), 0);
		tcp.listen(1, () -> {});

		final mainThread = Thread.current();
		var wokeAt:Null<Float> = null;
		final t0 = haxe.Timer.stamp();

		Thread.create(() -> {
			Sys.sleep(0.1);
			EventLoop.getThreadLoop(mainThread).run(() -> {
				wokeAt = haxe.Timer.stamp();
				tcp.close();
				async.done();
			});
		});

		while (wokeAt == null) {
			final wait = @:privateAccess mainLoop.getNextTick();
			mainLoop.loopOnce(true, wait);
			if (haxe.Timer.stamp() - t0 > 2.0) {
				tcp.close();
				Assert.fail("native wake did not deliver cross-thread event within 2s");
				async.done();
				return;
			}
		}

		final latency = wokeAt - t0;
		Assert.isTrue(latency >= 0.05 && latency < 0.5, 'unexpected wake latency: ${latency}s');
	}
	#end
}