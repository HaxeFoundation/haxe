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
}
