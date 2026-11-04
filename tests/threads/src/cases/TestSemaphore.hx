package cases;

import sys.thread.Semaphore;

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
}
