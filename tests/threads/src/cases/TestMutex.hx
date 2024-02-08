package cases;

import sys.thread.Mutex;

class TestMutex extends utest.Test {
	function testIssue10249() {
		var m = new Mutex();
		m.acquire();
		m.acquire();
		isTrue(m.tryAcquire());
		m.release();
		m.release();
		m.release();
	}
}