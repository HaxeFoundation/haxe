package cases;

import sys.thread.IThreadPool;
import sys.thread.ElasticThreadPool;

class TestElasticThreadPool extends misc.TestThreadPoolBase {
	function createThreadPool(count:Int):IThreadPool {
		return new ElasticThreadPool(count);
	}

	function testThreadTimeout() {
		var timeout = 0.1;
		var pool = new ElasticThreadPool(3, timeout);

		for(_ in 0...3)
			pool.run(() -> Sys.sleep(0.2));

		//by the end of this sleep all threads should be terminated as timed out
		Sys.sleep(1);

		equals(0, pool.threadsCount);

		//check we still can run tasks after all threads were terminated
		var lock = new Lock();
		for(_ in 0...3)
			pool.run(() -> {
				Sys.sleep(0.2);
				lock.release();
			});
		assertReleased(lock, 3);
	}
}