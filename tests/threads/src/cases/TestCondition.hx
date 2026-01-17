package cases;

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
		utest.Assert.pass();
	}
}
