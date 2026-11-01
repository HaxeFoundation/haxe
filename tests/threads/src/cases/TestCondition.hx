package cases;

import utest.Assert;
import sys.thread.Condition;
import sys.thread.Thread;

class TestCondition extends ThreadTestBase {
	function test() {
		final cond = new Condition();
		cond.acquire();
		Thread.create(() -> {
			cond.acquire();
			cond.signal();
			cond.release();
		});
		cond.wait();
		cond.release();
		Assert.pass();
	}

	function testTimedWaitTimesOut() {
		final cond = new Condition();
		cond.acquire();
		final start = haxe.Timer.stamp();
		Assert.isFalse(cond.wait(0.1));
		Assert.isTrue(haxe.Timer.stamp() - start >= 0.05);
		// the mutex must still be held by this thread after the timeout
		final lock = new sys.thread.Lock();
		var acquiredByOther = true;
		Thread.create(() -> {
			acquiredByOther = cond.tryAcquire();
			if (acquiredByOther) {
				cond.release();
			}
			lock.release();
		});
		Assert.isTrue(lock.wait(10.0));
		Assert.isFalse(acquiredByOther);
		cond.release();
	}

	function testTimedWaitSignaled() {
		final cond = new Condition();
		var ready = false;
		cond.acquire();
		Thread.create(() -> {
			cond.acquire();
			ready = true;
			cond.signal();
			cond.release();
		});
		final deadline = haxe.Timer.stamp() + 10.0;
		while (!ready) {
			final remaining = deadline - haxe.Timer.stamp();
			if (remaining <= 0 || !cond.wait(remaining)) {
				break;
			}
		}
		Assert.isTrue(ready);
		cond.release();
	}

	function testTimedWaitNonPositive() {
		final cond = new Condition();
		cond.acquire();
		Assert.isFalse(cond.wait(0));
		Assert.isFalse(cond.wait(-1));
		Assert.isFalse(cond.wait(Math.NaN));
		cond.release();
	}

	function testTimedWaitHuge() {
		final cond = new Condition();
		var ready = false;
		var timedOut = false;
		cond.acquire();
		Thread.create(() -> {
			cond.acquire();
			ready = true;
			cond.signal();
			cond.release();
		});
		// must neither overflow into an immediate timeout nor into a negative wait
		while (!ready) {
			if (!cond.wait(1e12)) {
				timedOut = true;
				break;
			}
		}
		Assert.isFalse(timedOut);
		Assert.isTrue(ready);
		cond.release();
	}

	function testUntimedWaitReturnsTrue() {
		final cond = new Condition();
		var ready = false;
		var result = false;
		cond.acquire();
		Thread.create(() -> {
			cond.acquire();
			ready = true;
			cond.signal();
			cond.release();
		});
		while (!ready) {
			result = cond.wait();
		}
		Assert.isTrue(result);
		cond.release();
	}

	function testTryAcquire() {
		final cond = new Condition();
		Assert.isTrue(cond.tryAcquire());
		// I would prefer to specify it to be not recursive...
		// Assert.isFalse(cond.tryAcquire());
		cond.release();
	}

	function testGCDuringWait() {
		final cond = new Condition();

		cond.acquire();
		Thread.create(() -> {
			cond.acquire();
			cond.signal();
			cond.wait();
			cond.release();
		});
		// wait for signal from thread
		cond.wait();

		#if cpp
		cpp.vm.Gc.run(true);
		#elseif hl
		hl.Gc.major();
		#elseif interp
		eval.vm.Gc.full_major();
		#elseif neko
		neko.vm.Gc.run(true);
		#end

		cond.signal();
		cond.release();

		utest.Assert.pass();
	}
}
