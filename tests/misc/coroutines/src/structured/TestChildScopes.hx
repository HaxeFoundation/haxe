package structured;

import haxe.coro.schedulers.VirtualTimeScheduler;

class TestChildScopes extends utest.Test {
	function test_waiting_for_child() {
		var result = 0;

		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			node.async(_ -> {
				delay(1000);

				result = 1;
			});
		});

		task.start();

		scheduler.advanceTo(999);
		Assert.isTrue(task.isActive());
		Assert.equals(result, 0);

		scheduler.advanceTo(1000);
		Assert.isFalse(task.isActive());
		Assert.equals(result, 1);
	}

	function test_deeply_nested_child() {
		var result = 0;

		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			node.async(node -> {
				node.async(node -> {
					node.async(_ -> {
						delay(1000);

						result = 1;
					});
				});
			});
		});

		task.start();

		scheduler.advanceTo(999);
		Assert.isTrue(task.isActive());
		Assert.equals(result, 0);

		scheduler.advanceTo(1000);
		Assert.isFalse(task.isActive());
		Assert.equals(result, 1);
	}

	function test_waiting_for_many_children() {
		final result    = [];
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			node.async(_ -> {
				delay(500);

				result.push(0);
			});

			node.async(_ -> {
				delay(1000);

				result.push(1);
			});
		});

		task.start();

		scheduler.advanceTo(499);
		Assert.same([], result);

		scheduler.advanceTo(500);
		Assert.same([ 0 ], result);

		scheduler.advanceTo(999);
		Assert.same([ 0 ], result);

		scheduler.advanceTo(1000);
		Assert.same([ 0, 1 ], result);

		Assert.isFalse(task.isActive());
	}

	function test_waiting_for_many_nested_children() {
		final result = [];

		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			node.async(node -> {
				node.async(_ -> {
					delay(500);

					result.push(0);
				});
			});

			node.async(_ -> {
				delay(1000);

				result.push(1);
			});
		});

		task.start();

		scheduler.advanceTo(499);
		Assert.same([], result);

		scheduler.advanceTo(500);
		Assert.same([ 0 ], result);

		scheduler.advanceTo(999);
		Assert.same([ 0 ], result);

		scheduler.advanceTo(1000);
		Assert.same([ 0, 1 ], result);

		Assert.isFalse(task.isActive());
	}

	function test_awaiting_child() {
		final expected = 'Hello, World';
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			final child = node.async(_ -> {
				delay(1000);

				return expected;
			});

			return child.await();
		});

		task.start();

		scheduler.advanceTo(1000);
		Assert.isFalse(task.isActive());
		Assert.equals(expected, task.get());
	}

	function test_awaiting_nested_child() {
		final expected = 'Hello, World';
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			final child = node.async(node -> {
				return
					node
						.async(_ -> {
							delay(1000);

							return expected;
						})
						.await();

			});

			return child.await();
		});

		task.start();

		scheduler.advanceTo(1000);
		Assert.isFalse(task.isActive());
		Assert.equals(expected, task.get());
	}

	function test_awaiting_single_child() {
		var result = 0;

		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			node.async(_ -> {
				delay(500);

				result = 1;
			});

			node
				.async(_ -> delay(1000))
				.await();
		});

		task.start();

		scheduler.advanceTo(499);
		Assert.isTrue(task.isActive());
		Assert.equals(result, 0);

		scheduler.advanceTo(500);
		Assert.isTrue(task.isActive());
		Assert.equals(result, 1);

		scheduler.advanceTo(1000);
		Assert.isFalse(task.isActive());
		Assert.equals(result, 1);
	}

	function test_awaiting_completed_child() {
		final expected  = 'Hello, World!';
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			final child = node.async(_ -> {
				yield();

				return expected;
			});

			delay(10);

			return child.await();
		});

		task.start();
		scheduler.advanceBy(10);

		Assert.isFalse(task.isActive());
		Assert.equals(expected, task.get());
	}
}