package cases;

#if !neko
import sys.thread.Condition;
import sys.thread.Thread;

class TestCondition extends utest.Test {
	function test() {
		final cond = new Condition();
		final thread = Thread.create(() -> {
			Sys.sleep(0.01);
			cond.acquire();
			cond.signal();
			cond.release();
		});
		cond.acquire();
		cond.wait();
		cond.release();
		utest.Assert.pass();
	}
}
#end
