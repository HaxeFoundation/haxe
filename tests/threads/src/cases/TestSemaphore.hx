package cases;

#if !neko
import sys.thread.Semaphore;
#end

class TestSemaphore extends utest.Test {
	#if !neko
	function test() {
		var m = new Semaphore(3);
		m.acquire();
		m.acquire();
		isTrue(m.tryAcquire());
		isFalse(m.tryAcquire());
		final t = Sys.time();
		isFalse(m.tryAcquire(0.1));
		isTrue(Sys.time() - t < 0.15);
		m.release();
		m.release();
		m.release();
	}
	#end
}
