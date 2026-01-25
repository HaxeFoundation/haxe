package cases;

import utest.Assert;
import sys.thread.Condition;
import sys.thread.Thread;

class TestCondition extends utest.Test {
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
