package cases;

import haxe.atomic.AtomicInt;
import sys.thread.Semaphore;
import utest.Assert;
import sys.thread.Condition;

class TestThread extends ThreadTestBase {
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

	function testAddCallbacks() {
		final stack = [];

		// register
		final handle = Thread.addCallbacks({
			onStart: () -> {
				stack.push(Thread.current());
			}
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

		function onCurrentExit(f:() -> Void) {
			return Thread.addCurrentCallbacks({onExit: f});
		}

		// 1 active onExit
		final thread = executeSync(() -> {
			onCurrentExit(() -> {
				threadVars[0] = Thread.current();
			});
		});
		Assert.isTrue(thread == threadVars[0]);

		// 1 onExit that gets closed
		final thread = executeSync(() -> {
			final handle = onCurrentExit(() -> {
				threadVars[0] = Thread.current();
			});
			handle.close();
		});
		Assert.isFalse(thread == threadVars[0]);

		// 2 onExit, first closed
		final thread = executeSync(() -> {
			final handle1 = onCurrentExit(() -> {
				threadVars[0] = Thread.current();
			});
			final handle2 = onCurrentExit(() -> {
				threadVars[1] = Thread.current();
			});
			handle1.close();
		});
		Assert.isFalse(thread == threadVars[0]);
		Assert.isTrue(thread == threadVars[1]);

		// 2 onExit, second closed
		final thread = executeSync(() -> {
			final handle1 = onCurrentExit(() -> {
				threadVars[0] = Thread.current();
			});
			final handle2 = onCurrentExit(() -> {
				threadVars[1] = Thread.current();
			});
			handle2.close();
		});
		Assert.isTrue(thread == threadVars[0]);
		Assert.isFalse(thread == threadVars[1]);

		// 3 onExit, second closed
		final thread = executeSync(() -> {
			final handle1 = onCurrentExit(() -> {
				threadVars[0] = Thread.current();
			});
			final handle2 = onCurrentExit(() -> {
				threadVars[1] = Thread.current();
			});
			final handle3 = onCurrentExit(() -> {
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

	function testOnAbortExceptionStillCallsOnExit() {
		// If onAbort throws, onExit should still be called
		final sem = new Semaphore(0);
		var onExitCalled = false;

		Thread.create(() -> {
			throw "job error";
		}, {
			onAbort: (_) -> {
				throw "onAbort error";
			},
			onExit: () -> {
				onExitCalled = true;
				sem.release();
			}
		});

		sem.acquire();
		Assert.isTrue(onExitCalled);
	}

	function testOnExitExceptionDoesNotCallOnAbort() {
		// If onExit throws, the custom onAbort callback should NOT be called again
		final sem = new Semaphore(0);
		var onAbortCalledCount = 0;

		Thread.create(() -> {
			throw "job error";
		}, {
			onAbort: (_) -> {
				onAbortCalledCount++;
			},
			onExit: () -> {
				try {
					throw "onExit error";
				} catch (e:Dynamic) {
					// Release the semaphore even when throwing so we can synchronize
					sem.release();
					throw e;
				}
			}
		});

		sem.acquire();
		// onAbort should have been called exactly once (for the job exception),
		// not again for the onExit exception (which goes to the default handler)
		Assert.equals(1, onAbortCalledCount);
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

	function testAddCallbacksHandle() {
		// Closing the addCallbacks handle prevents callbacks even for already-running threads
		var onExitCalled = false;
		final sem1 = new Semaphore(0);
		final sem2 = new Semaphore(0);
		final sem3 = new Semaphore(0);

		final handle = Thread.addCallbacks({
			onExit: () -> {
				onExitCalled = true;
			}
		});

		// Create a thread that signals when running then waits before exiting
		Thread.create(() -> {
			sem1.release(); // thread is running
			sem2.acquire(); // wait for permission to exit
		}, {onExit: () -> sem3.release()});

		sem1.acquire(); // thread is now running
		handle.close(); // close handle while the thread is still alive
		sem2.release(); // let the thread exit
		sem3.acquire(); // wait for thread to fully exit

		Assert.isFalse(onExitCalled);
	}

	function testAddCallbacksHandleCloseInOnExit() {
		// handle.close() from within the per-thread onExit callback
		// should prevent the global callback from being called
		var onExitCalled = false;
		final sem = new Semaphore(0);

		final handle = Thread.addCallbacks({
			onExit: () -> {
				onExitCalled = true;
			}
		});

		// The per-thread onExit closes the handle before global callbacks run
		Thread.create(() -> {}, {
			onExit: () -> {
				handle.close();
				sem.release();
			},
			onAbort: (_) -> {}
		});

		sem.acquire();
		Assert.isFalse(onExitCalled);
	}

	function testCallbackLIFOOrder() {
		// Callbacks should be called in LIFO order (last registered first)
		final sem = new Semaphore(0);
		final order = [];

		// handle1 registered first — runs LAST in LIFO order
		final handle1 = Thread.addCallbacks({
			onExit: () -> {
				order.push(1);
				sem.release();
			}
		});
		final handle2 = Thread.addCallbacks({onExit: () -> order.push(2)});
		final handle3 = Thread.addCallbacks({onExit: () -> order.push(3)});

		Thread.create(() -> {}, {onAbort: (_) -> {}});
		sem.acquire();

		handle1.close();
		handle2.close();
		handle3.close();

		Assert.same([3, 2, 1], order);
	}

	function testMultipleAddCallbacks() {
		// Multiple addCallbacks registrations should all be called
		final sem = new Semaphore(0);
		var count = 0;

		final handle1 = Thread.addCallbacks({onStart: () -> { count++; sem.release(); }});
		final handle2 = Thread.addCallbacks({onStart: () -> count++});

		Thread.create(() -> {}, {onAbort: (_) -> {}});
		sem.acquire();

		handle1.close();
		handle2.close();
		Assert.equals(2, count);
	}

	function testAddCallbacksGlobal() {
		// addCallbacks registers global callbacks applied to each new thread
		final sem = new Semaphore(0);
		var jobDoneThread:Null<Thread> = null;
		var exitThread:Null<Thread> = null;

		final handle = Thread.addCallbacks({
			onJobDone: () -> {
				jobDoneThread = Thread.current();
			},
			onExit: () -> {
				exitThread = Thread.current();
				sem.release();
			}
		});

		final thread = Thread.create(() -> {}, {onAbort: (_) -> {}});
		sem.acquire();
		handle.close();

		Assert.isTrue(thread == jobDoneThread);
		Assert.isTrue(thread == exitThread);
	}

	function testOnCreate() {
		// onCreate is called from the creating thread, not the new thread
		final sem = new Semaphore(0);
		final creatingThread = Thread.current();
		var onCreateThread:Null<Thread> = null;

		final handle = Thread.addCallbacks({
			onCreate: () -> {
				onCreateThread = Thread.current();
			}
		});

		Thread.create(() -> {}, {
			onExit: () -> sem.release(),
			onAbort: (_) -> {}
		});
		sem.acquire();
		handle.close();

		Assert.isTrue(creatingThread == onCreateThread);
	}

	function testOnCreatePerThread() {
		// Per-thread onCreate is also called from the creating thread
		final sem = new Semaphore(0);
		final creatingThread = Thread.current();
		var onCreateThread:Null<Thread> = null;

		Thread.create(() -> {}, {
			onCreate: () -> {
				onCreateThread = Thread.current();
			},
			onExit: () -> sem.release(),
			onAbort: (_) -> {}
		});
		sem.acquire();

		Assert.isTrue(creatingThread == onCreateThread);
	}

	function testOnCreateFiresBeforeThreadStarts() {
		// onCreate fires before onStart (i.e., before the thread begins executing)
		final sem = new Semaphore(0);
		final order = [];

		final handle = Thread.addCallbacks({
			onCreate: () -> order.push("onCreate"),
			onStart: () -> order.push("onStart"),
			onExit: () -> sem.release()
		});

		Thread.create(() -> {}, {onAbort: (_) -> {}});
		sem.acquire();
		handle.close();

		Assert.isTrue(order.indexOf("onCreate") < order.indexOf("onStart"));
	}
}