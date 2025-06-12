package structured;

import haxe.coro.ICancellableContinuation;
import haxe.coro.schedulers.VirtualTimeScheduler;
import haxe.coro.cancellation.CancellationToken;
import haxe.exceptions.ArgumentException;
import haxe.exceptions.CancellationException;

class TestCancellingSuspend extends utest.Test {
	function test_callback() {
		final actual    = [];
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			timeout(100, _ -> {
				suspendCancellable(cont -> {
					cont.onCancellationRequested = _ -> {
						actual.push(scheduler.now());
					}
				});
			});
		});

		task.start();

		scheduler.advanceBy(100);

		Assert.equals(1, actual.length);
		Assert.isTrue(100i64 == actual[0]);
		Assert.isFalse(task.isActive());
		Assert.isOfType(task.getError(), CancellationException);
	}

	function test_resuming_successfully() {
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			AssertAsync.raises(() -> {
				suspendCancellable(cont -> {
					scheduler.schedule(0, () -> {
						cont.resume(null, null);
					});
				});
			}, CancellationException);
		});

		task.start();
		task.cancel();

		scheduler.advanceBy(0);

		Assert.isFalse(task.isActive());
		Assert.isOfType(task.getError(), CancellationException);
	}

	function test_failing() {
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			AssertAsync.raises(() -> {
				suspendCancellable(cont -> {
					scheduler.schedule(0, () -> {
						cont.resume(null, new ArgumentException(''));
					});
				});
			}, CancellationException);
		});

		task.start();
		task.cancel();

		scheduler.advanceBy(0);

		Assert.isFalse(task.isActive());
		Assert.isOfType(task.getError(), CancellationException);
	}

	function test_callback_is_unregistered() {
		final actual    = [];
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			suspendCancellable(cont -> {
				cont.onCancellationRequested = _ -> {
					Assert.fail('should not be invoked');
				}
				cont.resume(null, null);
			});

			delay(1000);
		});

		task.start();

		scheduler.advanceBy(100);

		task.cancel();

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.isOfType(task.getError(), CancellationException);
	}

	function test_immediate_callback_execution() {
		var stashed : ICancellableContinuation<Any> = null;

		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			suspendCancellable(cont -> {
				stashed = cont;

				cont.resume(null, null);
			});

			node.context.get(hxcoro.task.CoroTask).cancel();

			final actual = [];

			stashed.onCancellationRequested = _ -> {
				actual.push('hello');
			}

			Assert.same([ 'hello' ], actual);
		});

		task.start();

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
	}

	function test_disallow_multiple_callback_assignments() {
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			suspendCancellable(cont -> {
				cont.onCancellationRequested = _ -> {
					trace('foo');
				}

				Assert.raises(() -> {
					cont.onCancellationRequested = _ -> {
						trace('foo');
					}
				});

				cont.resume(null, null);
			});
		});

		task.start();
		task.cancel();

		scheduler.advanceBy(0);

		Assert.isFalse(task.isActive());
		Assert.isOfType(task.getError(), CancellationException);
	}
}
