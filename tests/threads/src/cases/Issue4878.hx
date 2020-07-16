package cases;

import utest.Async;
import utest.Assert;
import utest.ITest;

class Issue4878 implements ITest {
	public function new() { }

	#if (java || python)

	@:timeout(5000)
	function test(async:Async) {
		Thread.create(() -> {
			var mutex = new Mutex();
			Thread.create(function() {
				mutex.acquire();
				mutex.acquire();
				mutex.release();
				Sys.sleep(.2);
				mutex.release();
			});
			Sys.sleep(0.05);
			Assert.isFalse(mutex.tryAcquire());
			Sys.sleep(.3);
			Assert.isTrue(mutex.tryAcquire());
			async.done();
		});
	}

	#end
}