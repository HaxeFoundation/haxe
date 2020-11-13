package cases;

import sys.thread.ThreadPool;
import sys.thread.ThreadPoolException;
import sys.thread.Lock;
import sys.thread.Deque;

@:timeout(10000)
class TestThreadPool extends utest.Test {
	function testCreateRunShutdown() {
		var threadsCount = 5;
		var tasksCount = 10;
		var lock = new Lock();
		var threads = new Deque();

		var pool = new ThreadPool(threadsCount);
		for(_ in 0...tasksCount) {
			pool.run(() -> {
				Sys.sleep(0.5); // keep the thread busy until all the tasks are submitted
				threads.add(Thread.current());
				lock.release();
			});
		}

		//wait for all tasks to finish
		for(_ in 0...tasksCount) {
			if(!lock.wait(2.0)) {
				fail('Timeout waiting for tasks to finish');
				return;
			}
		}

		//check we actually have `threadsCount` unique threads
		var uniqueThreads = [];
		var tasksExecuted = 0;
		while(true) {
			switch threads.pop(false) {
				case null:
					break;
				case thread:
					++tasksExecuted;
					if(!Lambda.exists(uniqueThreads, (t:Thread) -> t == thread)) {
						uniqueThreads.push(thread);
					}
			}
		}
		equals(tasksCount, tasksExecuted);
		equals(threadsCount, uniqueThreads.length);

		pool.shutdown();
		isTrue(pool.isShutdown);
	}

	function testShutdownRun_exception() {
		var pool = new ThreadPool(1);
		pool.shutdown();
		try {
			pool.run(() -> {});
			fail();
		} catch(e:ThreadPoolException) {
			pass();
		}
	}

	function testMultipleShutdown() {
		var pool = new ThreadPool(1);
		pool.shutdown();
		pool.shutdown();
		pass();
	}
}