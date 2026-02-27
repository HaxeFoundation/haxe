package cases;

import sys.thread.Semaphore;
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
		}, { onAbort: function(error) {
			exc = error;
			failingThread = Thread.current();
			cond.acquire();
			cond.signal();
			cond.release();
		}});
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
		}, { onExit: function() {
			exitingThread = Thread.current();
			cond.acquire();
			cond.signal();
			cond.release();
		}});
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
		}, { onExit: function() {
			acc.push("onExit");
			exitingThread = Thread.current();
			cond.acquire();
			cond.signal();
			cond.release();
		}, onAbort: function(error) {
			acc.push("onError");
			exc = error;
			failingThread = Thread.current();
		}});
		cond.wait();
		cond.release();

		Assert.isTrue(thread == failingThread);
		Assert.isTrue(thread == exitingThread);
		Assert.equals("error", exc.message);
		Assert.same(["onError", "onExit"], acc);
	}

	function executeSync(f:() -> Void) {
		final sem = new Semaphore(0);
		final cond = new Condition();
		cond.acquire();
		final thread = Thread.create(() -> {
			cond.acquire();
			f();
			cond.release();
		}, {onExit :() -> {
			sem.release();
		}});
		cond.signal();
		cond.release();
		sem.acquire();
		return thread;
	}

	function testOnJobStart() {
		final stack = [];

		// register
		final handle = Thread.onJobStart(callbacks -> {
			stack.push(Thread.current());
		});

		// spawn thread to check if we have it
		final thread = executeSync(() -> {});
		Assert.isTrue(thread == stack.pop());

		// close handle and try again
		handle.close();

		final thread = executeSync(() -> {});
		Assert.equals(0, stack.length);
	}

	function testOnCurrentExit() {
		var threadVars = [];

		// 1 active onExit
		final thread = executeSync(() -> {
			Thread.onCurrentExit(() -> {
				threadVars[0] = Thread.current();
			});
		});
		Assert.isTrue(thread == threadVars[0]);

		// 1 onExit that gets closed
		final thread = executeSync(() -> {
			final handle = Thread.onCurrentExit(() -> {
				threadVars[0] = Thread.current();
			});
			handle.close();
		});
		Assert.isFalse(thread == threadVars[0]);

		// 2 onExit, first closed
		final thread = executeSync(() -> {
			final handle1 = Thread.onCurrentExit(() -> {
				threadVars[0] = Thread.current();
			});
			final handle2 = Thread.onCurrentExit(() -> {
				threadVars[1] = Thread.current();
			});
			handle1.close();
		});
		Assert.isFalse(thread == threadVars[0]);
		Assert.isTrue(thread == threadVars[1]);

		// 2 onExit, second closed
		final thread = executeSync(() -> {
			final handle1 = Thread.onCurrentExit(() -> {
				threadVars[0] = Thread.current();
			});
			final handle2 = Thread.onCurrentExit(() -> {
				threadVars[1] = Thread.current();
			});
			handle2.close();
		});
		Assert.isTrue(thread == threadVars[0]);
		Assert.isFalse(thread == threadVars[1]);

		// 3 onExit, second closed
		final thread = executeSync(() -> {
			final handle1 = Thread.onCurrentExit(() -> {
				threadVars[0] = Thread.current();
			});
			final handle2 = Thread.onCurrentExit(() -> {
				threadVars[1] = Thread.current();
			});
			final handle3 = Thread.onCurrentExit(() -> {
				threadVars[2] = Thread.current();
			});
			handle2.close();
		});
		Assert.isTrue(thread == threadVars[0]);
		Assert.isFalse(thread == threadVars[1]);
		Assert.isTrue(thread == threadVars[2]);
	}

	function testOnJobDone() {
		// onJobDone should be called after a successful job
		final sem = new Semaphore(0);
		var jobDoneThread:Null<Thread> = null;

		final thread = Thread.create(() -> {}, {
			onJobDone: () -> {
				jobDoneThread = Thread.current();
			},
			onExit: () -> {
				sem.release();
			}
		});
		sem.acquire();
		Assert.isTrue(thread == jobDoneThread);
	}

	function testOnJobDoneNotCalledOnException() {
		// onJobDone should NOT be called when the thread throws
		final sem = new Semaphore(0);
		var jobDoneCalled = false;

		Thread.create(() -> {
			throw "error";
		}, {
			onJobDone: () -> {
				jobDoneCalled = true;
			},
			onAbort: (_) -> {},
			onExit: () -> {
				sem.release();
			}
		});
		sem.acquire();
		Assert.isFalse(jobDoneCalled);
	}

	function testOnJobDoneHandle() {
		// onJobDone via ThreadInstanceCallbacks returns a handle that can deregister it
		final sem = new Semaphore(0);
		var jobDoneCalled = false;

		final jobStartHandle = Thread.onJobStart(callbacks -> {
			final handle = callbacks.onJobDone(() -> {
				jobDoneCalled = true;
			});
			handle.close();
		});

		final thread = executeSync(() -> {});
		jobStartHandle.close();
		Assert.isFalse(jobDoneCalled);
	}

	function testMultipleOnJobStart() {
		// Multiple onJobStart handlers should all be called
		final sem = new Semaphore(0);
		var count = 0;

		final handle1 = Thread.onJobStart(_ -> count++);
		final handle2 = Thread.onJobStart(_ -> { count++; sem.release(); });

		Thread.create(() -> {}, {onAbort: (_) -> {}});
		sem.acquire();

		handle1.close();
		handle2.close();
		Assert.equals(2, count);
	}

	function testOnJobStartWithInstanceCallbacks() {
		// onJobStart can register per-thread callbacks via ThreadInstanceCallbacks
		final sem = new Semaphore(0);
		var jobDoneThread:Null<Thread> = null;
		var exitThread:Null<Thread> = null;

		final jobStartHandle = Thread.onJobStart(callbacks -> {
			callbacks.onJobDone(() -> {
				jobDoneThread = Thread.current();
			});
			callbacks.onExit(() -> {
				exitThread = Thread.current();
				sem.release();
			});
		});

		final thread = Thread.create(() -> {}, {onAbort: (_) -> {}});
		sem.acquire();
		jobStartHandle.close();

		Assert.isTrue(thread == jobDoneThread);
		Assert.isTrue(thread == exitThread);
	}
}