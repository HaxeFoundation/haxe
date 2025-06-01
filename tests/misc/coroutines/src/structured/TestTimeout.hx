package structured;

import haxe.Exception;
import haxe.exceptions.ArgumentException;
import haxe.coro.schedulers.VirtualTimeScheduler;
import hxcoro.exceptions.TimeoutException;

class TestTimeout extends utest.Test {
	function test_timeout() {
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			return timeout(500, _ -> {
				delay(1000);

				return 10;
			});
		});

		task.start();

		scheduler.advanceBy(500);

		Assert.isFalse(task.isActive());
		Assert.isOfType(task.getError(), TimeoutException);
	}

	function test_timeout_result() {
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			return timeout(1000, _ -> {
				delay(500);

				return 10;
			});
		});

		task.start();

		scheduler.advanceBy(500);

		Assert.isFalse(task.isActive());
		Assert.equals(10, task.get());
	}

	function test_zero_timeout() {
		final result    = [];
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			return timeout(0, _ -> {
				result.push(0);
			});
		});

		task.start();

		scheduler.advanceBy(0);

		Assert.isFalse(task.isActive());
		Assert.same([], result);
	}

	function test_negative_timeout() {
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			return timeout(-1, _ -> {
				delay(1000);
			});
		});

		task.start();

		scheduler.advanceBy(0);

		Assert.isFalse(task.isActive());
		Assert.isOfType(task.getError(), ArgumentException);
	}

	function test_timeout_does_not_propagate_cancellation() {
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			node.async(_ -> {
				try {
					timeout(500, _ -> {
						delay(1000);
					});
				} catch (_) {}
			});

			return node.async(_ -> {
				delay(100);

				return 10;
			}).await();
		});

		task.start();

		scheduler.advanceBy(500);

		Assert.isFalse(task.isActive());
		Assert.equals(10, task.get());
	}
}