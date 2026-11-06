package cases;

import utest.Assert;
import sys.thread.Semaphore;

@:timeout(2000)
class TestLock extends ThreadTestBase {
	function testLockContention(async:Async) {
		var main = Thread.current();
		final numThreads = 10;
		final sem = new Semaphore(0);
		for (n in 0...numThreads) {
			Thread.create(() -> {
				var l = new Lock();
				for (i in 0...50) {
					l.wait(0.01);
				}
				sem.release();
			});
		}

		for (i in 0...numThreads) {
			sem.acquire();
		}
		Assert.pass();
		async.done();
	}

	function testLockNoTimeout(async:Async) {
		final lock = new Lock();
		Thread.create(() -> {
			lock.release();
		});
		isTrue(lock.wait());
		async.done();
	}

	function testTimeoutNonPositive(async:Async) {
		final l = new Lock();
		isFalse(l.wait(0));
		isFalse(l.wait(-1));
		isFalse(l.wait(Math.NaN));
		async.done();
	}

	function testTimeoutHuge(async:Async) {
		final l = new Lock();
		final ready = new Lock();

		Thread.create(() -> {
			ready.release();
			// try to give time for the acquire to happen before we release, to detect an immediate timeout
			Sys.sleep(0.01);
			l.release();
		});
		ready.wait();
		// must neither overflow into an immediate timeout nor into a negative wait
		isTrue(l.wait(1e12));
		async.done();
	}
}
