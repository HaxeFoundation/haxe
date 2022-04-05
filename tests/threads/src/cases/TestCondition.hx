package cases;

#if !neko
import sys.thread.Condition;
import sys.thread.Thread;
#end

class TestCondition extends utest.Test {
	#if !neko
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
	#end
}
