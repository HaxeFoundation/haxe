package concurrent;

import hxcoro.concurrent.CoroSemaphore;
import haxe.coro.schedulers.VirtualTimeScheduler;
import haxe.coro.Mutex;
import hxcoro.concurrent.CoroMutex;

class TestMutex extends utest.Test {
	function testSimple() {
		final m = new Mutex();
        m.acquire();
        m.release();
        Assert.equals(true, m.tryAcquire());
        m.release();
	}

	function testPromptCancellation() {
		var scheduler = new VirtualTimeScheduler();
		final lines = [];
		function report(s:String) {
			final now = scheduler.nowMs();
			lines.push('$now: $s');
		}
		final task = CoroRun.with(scheduler).create(node -> {
			final m = new CoroMutex();

			node.async(_ -> {
				report("0 acquiring");
				m.acquire();
				report("0 acquired");
				delay(1000);

				m.release();
				report("0 released");
			});

			node.async(_ -> {
				try {
					timeout(500, _ -> {
						report("1 acquiring");
						m.acquire();
						report('1 acquired');
						m.release();
						report("1 released");
					});
				} catch (_) {
					report("1 timeout");
				}
			});

			node.async(_ -> {
				report("2 acquiring");
				m.acquire();
				report("2 acquired");
				m.release();
				report("2 released");
			});
		});
		task.start();
		while (task.isActive()) {
			scheduler.advanceBy(1);
		}
		Assert.same([
			   "0: 0 acquiring",
			   "0: 0 acquired",
			   "0: 1 acquiring",
			   "0: 2 acquiring",
			 "500: 1 timeout",
			"1000: 0 released",
			"1000: 2 acquired",
			"1000: 2 released",
		], lines);
	}

	function testSemaphoreAcquire() {
		var scheduler = new VirtualTimeScheduler();
		final numTasks = 500;
		final numTasksHalved = Std.int(numTasks / 2);
		var numTasksCompleted = 0;
		final task = CoroRun.with(scheduler).create(node -> {
			final m = new CoroSemaphore(numTasksHalved);
			for (_ in 0...numTasks) {
				node.async(_ -> {
					m.acquire();
					delay(500);
					m.release();
					numTasksCompleted++;
				});
			}
		});
		task.start();
		scheduler.advanceTo(499);
		Assert.equals(0, numTasksCompleted);
		scheduler.advanceTo(500);
		Assert.equals(numTasksHalved, numTasksCompleted);
		scheduler.advanceTo(999);
		Assert.equals(numTasksHalved, numTasksCompleted);
		scheduler.advanceTo(1000);
		Assert.equals(numTasks, numTasksCompleted);
	}

	function testSemaphoreTryAcquire() {
		var scheduler = new VirtualTimeScheduler();
		final numTasks = 500;
		final numTasksHalved = Std.int(numTasks / 2);
		var numTasksCompleted = 0;
		var numEarlyAcquires = 0;
		var numLateAcquires = 0;
		final task = CoroRun.with(scheduler).create(node -> {
			final m = new CoroSemaphore(numTasksHalved);
			for (i in 0...numTasks) {
				node.async(_ -> {
					final odd = i & 1 == 1;
					delay(odd ? 1 : 0);
					if (m.tryAcquire()) {
						numEarlyAcquires++;
						delay(odd ? 0 : 1);
					} else {
						delay(odd ? 0 : 1);
						Assert.isTrue(m.tryAcquire());
						numLateAcquires++;
					}
					m.release();
					numTasksCompleted++;
				});
			}
		});
		task.start();
		while (task.isActive()) {
			scheduler.advanceBy(1);
		}
		Assert.equals(numTasks, numTasksCompleted);
		Assert.equals(numTasksHalved, numEarlyAcquires);
		Assert.equals(numTasksHalved, numLateAcquires);
	}
}
