package cases;

import utest.Assert;
import haxe.EventLoop;
import haxe.EventLoopDriver;
import haxe.HaxeEventLoopDriver;

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

	/**
		Sync `swapDriver` on the loop thread before `loop()` applies immediately
		and runs `onSwapped` on that thread.
	**/
	function testSwapDriverSync() {
		final loop = new EventLoop();
		final newDriver = new HaxeEventLoopDriver();
		var swappedOn = null;
		loop.swapDriver(newDriver, () -> swappedOn = Thread.current());
		isTrue(loop.getDriver() == newDriver);
		isNull(loop.getPendingDriver());
		equals(Thread.current(), swappedOn);
	}

	/**
		Cross-thread `swapDriver` while the loop thread is blocked in `wait(0)`
		must defer, wake, apply on the loop thread, and run `onSwapped` there.
	**/
	function testSwapDriverWhileBlocked() {
		final ready = new Lock();
		final swapped = new Lock();
		var loopThread:Thread = null;
		var onSwappedThread:Thread = null;
		final newDriver = new HaxeEventLoopDriver();

		final child = Thread.create(() -> {
			loopThread = Thread.current();
			EventLoop.current.promise();
			ready.release();
		});

		isTrue(ready.wait(1.0));
		Sys.sleep(0.05); // let onJobDone enter loop() → wait(0)

		final loop = EventLoop.getThreadLoop(child);
		isTrue(loop != null);
		isTrue(loop.getDriver() != newDriver);

		loop.swapDriver(newDriver, () -> {
			onSwappedThread = Thread.current();
			isTrue(loop.getDriver() == newDriver);
			loop.deliver();
			swapped.release();
		});

		isTrue(swapped.wait(1.0));
		equals(loopThread, onSwappedThread);
		isTrue(loop.getDriver() == newDriver);
		isNull(loop.getPendingDriver());
	}

	/**
		Last-wins: a second pending `swapDriver` closes the superseded pending
		driver and drops its `onSwapped` callback.
	**/
	function testSwapDriverLastWins() {
		final loop = new EventLoop();
		final first = new TrackingDriver();
		final second = new TrackingDriver();
		var firstCb = false;
		var secondOn:Thread = null;

		loop.run(() -> {
			loop.swapDriver(first, () -> firstCb = true);
			isTrue(loop.getPendingDriver() == first);
			loop.swapDriver(second, () -> secondOn = Thread.current());
			isTrue(first.closed);
			isFalse(second.closed);
			isTrue(loop.getPendingDriver() == second);
		});
		loop.loopOnce();

		isFalse(firstCb);
		equals(Thread.current(), secondOn);
		isTrue(loop.getDriver() == second);
		isNull(loop.getPendingDriver());
		loop.dispose();
	}

	/**
		After `dispose`, cross-thread `wakeup` / `run` must not NPE on the
		closed non-null driver sentinel.
	**/
	function testDisposeThenCrossThreadWakeup() {
		final loop = new EventLoop();
		loop.dispose();
		final done = new Lock();
		Thread.create(() -> {
			loop.run(() -> {});
			@:privateAccess loop.wakeup();
			loop.getDriver().wake();
			done.release();
		});
		isTrue(done.wait(1.0));
		loop.getDriver().close(); // idempotent
	}

	#if (hl && hl_ver >= version("1.16.0"))
	/**
		`getFromEventLoop` returns a Loop synchronously, installs via `swapDriver`,
		and is idempotent for current/pending UV drivers.
	**/
	function testGetFromEventLoopAttach() {
		final loop = new EventLoop();
		final uv1 = hl.uv.Loop.getFromEventLoop(loop);
		isTrue(Std.isOfType(loop.getDriver(), hl.uv.UvEventLoopDriver));
		isTrue((cast loop.getDriver() : hl.uv.UvEventLoopDriver).uvLoop == uv1);
		final uv2 = hl.uv.Loop.getFromEventLoop(loop);
		isTrue(uv1 == uv2);
		isTrue(Std.isOfType(loop.getDriver(), hl.uv.UvEventLoopDriver));
		isNull(loop.getPendingDriver());
		loop.dispose();
	}

	/**
		Deferred attach from an event callback: pending UV swap keeps the loop
		alive until apply; second getFromEventLoop reuses the pending driver.
	**/
	function testGetFromEventLoopDeferredIdempotent() {
		final loop = new EventLoop();
		var uvFromCallback:hl.uv.Loop = null;
		var uvSecond:hl.uv.Loop = null;
		var driverAfterFirst:haxe.EventLoopDriver = null;
		loop.run(() -> {
			uvFromCallback = hl.uv.Loop.getFromEventLoop(loop);
			driverAfterFirst = loop.getPendingDriver();
			isTrue(Std.isOfType(driverAfterFirst, hl.uv.UvEventLoopDriver));
			// Still Haxe driver until applyPendingSwap at end of loopOnce.
			isTrue(Std.isOfType(loop.getDriver(), HaxeEventLoopDriver));
			uvSecond = hl.uv.Loop.getFromEventLoop(loop);
			isTrue(uvFromCallback == uvSecond);
			isTrue(loop.getPendingDriver() == driverAfterFirst);
		});
		loop.loopOnce();
		isTrue(Std.isOfType(loop.getDriver(), hl.uv.UvEventLoopDriver));
		isTrue((cast loop.getDriver() : hl.uv.UvEventLoopDriver).uvLoop == uvFromCallback);
		isNull(loop.getPendingDriver());
		loop.dispose();
	}

	/**
		Cross-thread `getFromEventLoop` while a child is in `wait(0)` must not
		idle-exit before the UV driver applies (pending swap is external work).
	**/
	function testGetFromEventLoopCrossThreadAttach() {
		final ready = new Lock();
		final done = new Lock();
		var loopThread:Thread = null;

		final child = Thread.create(() -> {
			loopThread = Thread.current();
			EventLoop.current.promise();
			ready.release();
		});

		isTrue(ready.wait(1.0));
		Sys.sleep(0.05);

		final loop = EventLoop.getThreadLoop(child);
		final uv = hl.uv.Loop.getFromEventLoop(loop);
		isTrue(uv != null);

		loop.run(() -> {
			isTrue(Std.isOfType(EventLoop.current.getDriver(), hl.uv.UvEventLoopDriver));
			equals(loopThread, Thread.current());
			EventLoop.current.deliver();
			done.release();
		});

		isTrue(done.wait(2.0));
		isTrue(Std.isOfType(loop.getDriver(), hl.uv.UvEventLoopDriver));
		isNull(loop.getPendingDriver());
	}

	/**
		Rewrite of the old `testNativeWake` pattern: express wake via child
		`EventLoop.loop()` (after job) and cross-thread `run`, with a ref'd TCP
		listener as user work. Does not use blocking `loopOnce(maxBlock)`.
	**/
	@:timeout(3000)
	function testUvWakeWithSocket() {
		final ready = new Lock();
		final woke = new Lock();
		var tcp:hl.uv.Tcp = null;
		var wokeAt:Null<Float> = null;
		final t0 = haxe.Timer.stamp();

		final child = Thread.create(() -> {
			final loop = EventLoop.current;
			final uv = hl.uv.Loop.getFromEventLoop(loop);
			tcp = new hl.uv.Tcp(uv);
			tcp.bind(new sys.net.Host("127.0.0.1"), 0);
			tcp.listen(1, () -> {});
			ready.release();
			// job ends → onJobDone → loop(); TCP keeps hasExternalWork alive
		});

		isTrue(ready.wait(1.0));
		Sys.sleep(0.1);

		EventLoop.getThreadLoop(child).run(() -> {
			wokeAt = haxe.Timer.stamp();
			tcp.close();
			woke.release();
		});

		isTrue(woke.wait(2.0));
		final latency = wokeAt - t0;
		isTrue(latency >= 0.05 && latency < 0.5, 'unexpected wake latency: ${latency}s');
	}

	/**
		Busy-spin regression: UV-backed `wait(0)` with **no** ref'd user handle
		must block until the async doorbell wakes it (not return immediately).
	**/
	@:timeout(3000)
	function testUvIdleNoBusySpinWake() {
		final ready = new Lock();
		final finished = new Lock();
		var driver:EventLoopDriver = null;
		var elapsed = 0.0;

		Thread.create(() -> {
			final loop = EventLoop.current;
			hl.uv.Loop.getFromEventLoop(loop);
			driver = loop.getDriver();
			isTrue(Std.isOfType(driver, hl.uv.UvEventLoopDriver));
			isFalse(driver.hasExternalWork());
			ready.release();
			final t0 = haxe.Timer.stamp();
			// Direct driver.wait(0) on the owning thread — no TCP / user handle.
			driver.wait(0);
			elapsed = haxe.Timer.stamp() - t0;
			finished.release();
		});

		isTrue(ready.wait(1.0));
		Sys.sleep(0.1);
		driver.wake();
		isTrue(finished.wait(2.0));
		isTrue(elapsed >= 0.08, 'wait(0) returned too fast (busy-spin?): ${elapsed}s');
		isTrue(elapsed < 0.5, 'wait(0) took too long: ${elapsed}s');
	}

	/**
		Promise-idle path: UV driver with no user handle stays in `loop()` via
		`promise`, blocks, and wakes via cross-thread `run` + doorbell.
	**/
	@:timeout(3000)
	function testUvPromiseIdleWake() {
		final ready = new Lock();
		final done = new Lock();
		var wokeAt:Null<Float> = null;
		final t0 = haxe.Timer.stamp();

		final child = Thread.create(() -> {
			final loop = EventLoop.current;
			hl.uv.Loop.getFromEventLoop(loop);
			isFalse(loop.getDriver().hasExternalWork());
			loop.promise();
			ready.release();
			// job ends → loop() → wait(0) on UV with only unref'd doorbell/timer
		});

		isTrue(ready.wait(1.0));
		Sys.sleep(0.1);

		EventLoop.getThreadLoop(child).run(() -> {
			wokeAt = haxe.Timer.stamp();
			EventLoop.current.deliver();
			done.release();
		});

		isTrue(done.wait(2.0));
		final latency = wokeAt - t0;
		isTrue(latency >= 0.05 && latency < 0.5, 'unexpected promise-idle wake latency: ${latency}s');
	}

	/**
		Positive `maxBlock` with no user handle must honor the deadline timer
		(not spin-return from `UV_RUN_ONCE`).
	**/
	function testUvWaitDeadlineWithoutUserHandle() {
		final loop = new EventLoop();
		hl.uv.Loop.getFromEventLoop(loop);
		final driver = loop.getDriver();
		isFalse(driver.hasExternalWork());
		final t0 = haxe.Timer.stamp();
		driver.wait(0.1);
		final elapsed = haxe.Timer.stamp() - t0;
		isTrue(elapsed >= 0.08, 'wait(0.1) returned too fast: ${elapsed}s');
		isTrue(elapsed < 0.5, 'wait(0.1) took too long: ${elapsed}s');
		loop.dispose();
	}
	#end
}

/**
	Test helper: tracks `close` for last-wins `swapDriver` assertions.
**/
private class TrackingDriver implements EventLoopDriver {
	public final allowsReentrancy = true;
	public var closed = false;

	public function new() {}

	public function wait(maxBlock:Float):Void {}

	public function wake():Void {}

	public function close():Void {
		closed = true;
	}

	public function hasExternalWork():Bool {
		return false;
	}
}
