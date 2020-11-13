package misc;

import sys.thread.IThreadPool;
import sys.thread.ThreadPoolException;
import sys.thread.Deque;
import haxe.Timer;

abstract class TestThreadPoolBase extends utest.Test {

	abstract function createThreadPool(count:Int):IThreadPool;

	function assertReleased(lock:Lock, releaseCount:Int, ?timeout:Float = 2, ?pos:haxe.PosInfos) {
		var releases = 0;
		var timeouts = 0;
		var timeoutTime = Timer.stamp() + timeout;
		for(_ in 0...releaseCount) {
			var timeout = Math.max(timeoutTime - Timer.stamp(), 0.1);
			if(lock.wait(timeout))
				++releases
			else
				++timeouts;
		}
		isTrue(timeouts == 0 && releases == releaseCount, 'Lock was released $releases times out of $releaseCount with $timeouts timeouts', pos);
	}

	function testCreateRunShutdown() {
		var threadsCount = 5;
		var tasksCount = 10;
		var lock = new Lock();
		var threads = new Deque();

		var pool = createThreadPool(threadsCount);
		var taskIds = [];
		for(id in 0...tasksCount) {
			pool.run(() -> {
				Sys.sleep(0.2); // keep the thread busy until all the tasks are submitted
				threads.add(Thread.current());
				taskIds.push(id);
				lock.release();
			});
		}

		//wait for all tasks to finish
		assertReleased(lock, tasksCount);

		//check we had `tasksCount` unique tasks
		taskIds.sort(Reflect.compare);
		same([for(id in 0...tasksCount) id], taskIds);

		//check we've run our tasks in `threadsCount` unique threads
		var uniqueThreads = [];
		while(true) {
			switch threads.pop(false) {
				case null:
					break;
				case thread:
					if(!Lambda.exists(uniqueThreads, (t:Thread) -> t == thread)) {
						uniqueThreads.push(thread);
					}
			}
		}
		equals(threadsCount, pool.threadsCount);
		equals(threadsCount, uniqueThreads.length);

		pool.shutdown();
		isTrue(pool.isShutdown);
	}

	function testShutdown_finishesSubmittedTasks() {
		var tasksCount = 5;
		var pool = createThreadPool(3);
		var lock = new Lock();
		for(_ in 0...tasksCount) {
			pool.run(() -> {
				Sys.sleep(0.2);
				lock.release();
			});
		}
		pool.shutdown();

		assertReleased(lock, tasksCount);
	}

	function testShutdownRun_exception() {
		var pool = createThreadPool(1);
		pool.shutdown();
		try {
			pool.run(() -> {});
			fail();
		} catch(e:ThreadPoolException) {
			pass();
		}
	}

	function testMultipleShutdown() {
		var pool = createThreadPool(1);
		pool.run(() -> Sys.sleep(0.2));
		pool.shutdown();
		isTrue(pool.isShutdown);
		pool.shutdown();
		pass();
	}
}