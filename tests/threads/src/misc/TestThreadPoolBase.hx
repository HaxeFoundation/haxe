package misc;

import sys.thread.IThreadPool;
import sys.thread.ThreadPoolException;
import sys.thread.Deque;
import sys.thread.Mutex;
import haxe.Exception;
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
		var tasksCount = threadsCount * 2;
		var taskThreads = [];
		var taskIds = [];
		var lock = new Lock();
		var mutex = new Mutex();

		var pool = createThreadPool(threadsCount);
		for(id in 0...tasksCount) {
			pool.run(() -> {
				Sys.sleep(0.2); // keep the thread busy until all the tasks are submitted
				mutex.acquire();
				taskThreads.push(Thread.current());
				taskIds.push(id);
				mutex.release();
				lock.release();
			});
		}

		//wait for all tasks to finish
		assertReleased(lock, tasksCount);

		//check we had `tasksCount` unique tasks
		taskIds.sort(Reflect.compare);
		var expected = [for(id in 0...tasksCount) id];
		same(expected, taskIds, 'Expected $expected, but they are $taskIds');

		//check each thread run two tasks
		for(thread in taskThreads) {
			var count = 0;
			for(t in taskThreads)
				if(t == thread)
					++count;
			if(count != 2)
				fail('Some thread executed $count tasks instead of 2');
		}
		equals(threadsCount, pool.threadsCount);

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

	function testRunFor(async:Async) {
		var mainThread = Thread.current();
		var pool = createThreadPool(1);
		//result
		async.branch(async -> {
			pool.runFor(
				() -> {
					isFalse(mainThread == Thread.current());
					return 123;
				},
				(e, r) -> {
					if(e != null)
						fail(e.message);
					isTrue(mainThread == Thread.current());
					equals(123, r);
					async.done();
				}
			);
		});
		//exception
		async.branch(async -> {
			pool.runFor(
				() -> {
					isFalse(mainThread == Thread.current());
					throw new Exception('');
				},
				(e, r) -> {
					isOfType(e, Exception);
					isTrue(mainThread == Thread.current());
					async.done();
				}
			);
		});
	}
}