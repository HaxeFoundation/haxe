package cases;

import utest.Assert;
import utest.ITest;

class Issue3767 implements ITest {
	public function new() { }

	#if (java || python)

	@:timeout(5000)
	function testBasicLock(async:utest.Async) {
		Thread.create(() -> {
			var lock = new Lock();
			//it starts locked
			Assert.isFalse(lock.wait(0.001));
			lock.release();
			Assert.isTrue(lock.wait(.001));
			Assert.isFalse(lock.wait(.001));
			Assert.isFalse(lock.wait(.001));

			lock.release();
			Assert.isTrue(lock.wait());
			lock.release();
			lock.release();
			lock.release();
			Assert.isTrue(lock.wait());
			Assert.isTrue(lock.wait(.001));
			Assert.isTrue(lock.wait());
			Assert.isFalse(lock.wait(.001));

			var cur = Sys.time();
			Thread.create(function()
			{
				Sys.sleep(.01);
				lock.release();
			});
			Assert.isTrue(lock.wait(2.0));
			Assert.isTrue( (Sys.time() - cur) < 2 );
			Thread.create(function()
			{
				Sys.sleep(.01);
				lock.release();
			});
			Assert.isTrue(lock.wait());
			async.done();
		});
	}

	#end
}