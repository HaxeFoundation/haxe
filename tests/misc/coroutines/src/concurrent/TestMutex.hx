package concurrent;

import hxcoro.ds.channels.Channel;
import haxe.exceptions.CancellationException;
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
			final now = scheduler.now();
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

	function testMutexCancelling() {
		var scheduler = new VirtualTimeScheduler();
		final lines = [];
		function report(s:String) {
			final now = scheduler.now();
			lines.push('$now: $s');
		}
		final task = CoroRun.with(scheduler).create(node -> {
			final mutex1 = new CoroMutex();
			final mutex2 = new CoroMutex();
			final child1 = node.async(_ -> {
				report("1 acquiring 1");
				mutex1.acquire();
				report("1 acquired 1");
				delay(2);
				report("1 acquiring 2 (deadlock)");
				try {
					mutex2.acquire();
				} catch (e:CancellationException) {
					report("1 cancelled");
					mutex1.release();
					throw e;
				}
				report("1 acquired 2");
			});
			final child2 = node.async(_ -> {
				delay(1);
				report("2 acquiring 2");
				mutex2.acquire();
				report("2 acquired 2");
				delay(1);
				report("2 acquiring 1 (deadlock)");
				mutex1.acquire();
				report("2 acquired 1");
				report("2 releasing 1");
				mutex1.release();
				report("2 released 1");
				report("2 releasing 2");
				mutex2.release();
				report("2 released 2");
			});
			delay(3);
			report("parent cancelling 1");
			child1.cancel();
			report("parent cancelled 1");
			delay(1);
			report('1 active: ${child1.isActive()}');
			report('2 active: ${child2.isActive()}');
		});
		task.start();
		while (task.isActive()) {
			scheduler.advanceBy(1);
		}
		Assert.same([
			"0: 1 acquiring 1",
			"0: 1 acquired 1",
			"1: 2 acquiring 2",
			"1: 2 acquired 2",
			"2: 1 acquiring 2 (deadlock)",
			"2: 2 acquiring 1 (deadlock)",
			"3: parent cancelling 1",
			"3: parent cancelled 1",
			"3: 1 cancelled",
			"3: 2 acquired 1",
			"3: 2 releasing 1",
			"3: 2 released 1",
			"3: 2 releasing 2",
			"3: 2 released 2",
			"4: 1 active: false",
			"4: 2 active: false",
		], lines);
	}

	function testRandomSemaphoreCancelling() {
		for (semaphoreSize in [1, 2, 4, 8]) {
			for (numTasks in [1, 2, 10, 100]) {
				var scheduler = new VirtualTimeScheduler();
				var semaphore = new CoroSemaphore(semaphoreSize);
				var semaphoreHolders = Channel.createBounded({ size : 1 });
				var hangingMutex = new CoroMutex();
				final task = CoroRun.with(scheduler).create(node -> {
					hangingMutex.acquire();
					var numCompletedTasks = 0;
					for (_ in 0...numTasks) {
						node.async(node -> {
							delay(Std.random(15));
							semaphore.acquire();
							semaphoreHolders.writer.write(node);
							try {
								hangingMutex.acquire(); // will never succeed
							} catch(e:CancellationException) {
								semaphore.release();
								numCompletedTasks++;
								throw e;
							}
						});
					}
					delay(1);
					while (numCompletedTasks != numTasks) {
						var holder = semaphoreHolders.reader.read();
						holder.cancel();
						// this is weird, how do we wait here properly?
						yield();
						yield();
					}
					hangingMutex.release();
					numCompletedTasks;
				});
				task.start();
				while (task.isActive()) {
					scheduler.advanceBy(1);
				}
				Assert.equals(numTasks, task.get());
			}
		}
	}
}
