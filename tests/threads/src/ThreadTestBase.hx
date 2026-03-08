import haxe.atomic.AtomicInt;
import sys.thread.Semaphore;
import sys.thread.Condition;

class ThreadTestBase extends utest.Test {
	var activeThreads:AtomicInt;
	var semaphore:Semaphore;

	function setup() {
		activeThreads = new AtomicInt(0);
		semaphore = new Semaphore(0);
		Thread.addCallbacks({
			onStart: () -> {
				activeThreads.add(1);
			},
			onExit: () -> {
				semaphore.release();
			}
		});
	}

	function teardown() {
		final activeThreads = activeThreads.load();
		for (_ in 0...activeThreads) {
			semaphore.acquire();
		}
	}
}