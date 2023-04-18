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
		isFalse(m.tryAcquire(0.1));
		m.release();
		m.release();
		m.release();
	}
	#end
}
