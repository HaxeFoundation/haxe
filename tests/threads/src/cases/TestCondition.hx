package cases;

#if !neko
import sys.thread.Condition;
import sys.thread.Thread;
#end

class TestCondition extends utest.Test {
	#if !neko
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
	#end
}
