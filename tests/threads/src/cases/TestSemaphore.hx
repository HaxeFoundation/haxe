package cases;

import sys.thread.Semaphore;

@:timeout(2000)
class TestSemaphore extends ThreadTestBase {
	function test() {
		var m = new Semaphore(3);
		m.acquire();
		m.acquire();
		isTrue(m.tryAcquire());
		isFalse(m.tryAcquire());
		isFalse(m.tryAcquire(0.1));
		m.release();
		m.release();
		m.release();
	}

	function testTimeoutNonPositive(async:Async) {
		final m = new Semaphore(0);
		isFalse(m.tryAcquire(0));
		isFalse(m.tryAcquire(-1));
		isFalse(m.tryAcquire(Math.NaN));
		async.done();
	}

	function testTimeoutHuge(async:Async) {
		final m = new Semaphore(0);
		final ready = new Semaphore(0);

		Thread.create(() -> {
			ready.release();
			// try to give time for the acquire to happen before we release, to detect an immediate timeout
			Sys.sleep(0.01);
			m.release();
		});
		ready.acquire();
		// must neither overflow into an immediate timeout nor into a negative wait
		isTrue(m.tryAcquire(1e12));
		async.done();
	}
}
