package cases;

import utest.Async;
import utest.Assert;
import utest.ITest;

class Issue8063 implements ITest {
	public function new() { }

	@:timeout(5000)
	function test(async:Async) {
		Assert.isTrue(Thread.current() == Thread.current());
		Thread.create(() -> {
			Assert.isTrue(Thread.current() == Thread.current());
			async.done();
		});
	}
}