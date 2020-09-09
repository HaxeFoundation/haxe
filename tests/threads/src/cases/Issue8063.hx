package cases;

import utest.Assert;
import utest.ITest;

class Issue8063 implements ITest {
	public function new() { }

	function test() {
		var lock = new Lock();
		Assert.isTrue(Thread.current() == Thread.current());
		Thread.create(() -> {
			Assert.isTrue(Thread.current() == Thread.current());
			lock.release();
		});
		Assert.isTrue(lock.wait(2.0));
	}
}