import haxe.coro.Mutex;

class TestMutex extends utest.Test {
	function testSimple() {
		final m = new Mutex();
        m.acquire();
        m.release();
        Assert.equals(true, m.tryAcquire());
        m.release();
	}
}
