package cases;

import utest.Assert;
import sys.thread.Condition;

class TestThread extends utest.Test {
	function testOnAbort() {
		final cond = new Condition();
		var failingThread = null;
		var exc = null;
		cond.acquire();
		final thread = Thread.create(() -> {
			throw "error";
		}, function(error) {
			exc = error;
			failingThread = Thread.current();
			cond.acquire();
			cond.signal();
			cond.release();
		});
		cond.wait();
		cond.release();

		Assert.isTrue(thread == failingThread);
		Assert.equals("error", exc.message);
	}

	function testOnExit() {
		final cond = new Condition();
		var exitingThread = null;
		cond.acquire();
		final thread = Thread.create(() -> {
			throw "error";
		}, function() {
			exitingThread = Thread.current();
			cond.acquire();
			cond.signal();
			cond.release();
		});
		cond.wait();
		cond.release();

		Assert.isTrue(thread == exitingThread);
	}

	function testBoth() {
		final cond = new Condition();
		var exitingThread = null;
		var failingThread = null;
		var exc = null;
		var acc = [];
		cond.acquire();
		final thread = Thread.create(() -> {
			throw "error";
		}, function() {
			acc.push("onExit");
			exitingThread = Thread.current();
			cond.acquire();
			cond.signal();
			cond.release();
		}, function(error) {
			acc.push("onError");
			exc = error;
			failingThread = Thread.current();
		});
		cond.wait();
		cond.release();

		Assert.isTrue(thread == failingThread);
		Assert.isTrue(thread == exitingThread);
		Assert.equals("error", exc.message);
		Assert.same(["onError", "onExit"], acc);
	}
}