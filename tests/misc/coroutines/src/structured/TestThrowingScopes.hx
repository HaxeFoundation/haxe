package structured;

import haxe.Exception;
import haxe.coro.schedulers.VirtualTimeScheduler;

class FooException extends Exception {
	public function new() {
		super('foo');
	}
}

class TestThrowingScopes extends utest.Test {
	public function test_error_passes_up() {
		Assert.raises(() -> {
			CoroRun.runScoped(node -> {
				node.async(_ -> {
					throw new FooException();
				});
			});
		}, FooException);
	}

	public function test_error_passes_up_deep_nesting() {
		Assert.raises(() -> {
			CoroRun.runScoped(node -> {
				node.async(node -> {
					node.async(_ -> {
						throw new FooException();
					});
				});
			});
		}, FooException);
	}

	public function test_sibling_cancelled() {
		Assert.raises(() -> {
			CoroRun.runScoped(node -> {
				node.async(_ -> {
					while (true) {
						yield();
					}
				});

				throw new FooException();
			});
		}, FooException);
	}

	public function test_recursive_children_cancelled_non_suspending_root() {
		Assert.raises(() -> {
			CoroRun.runScoped(node -> {
				node.async(node -> {
					node.async(node -> {
						while (true) {
							yield();
						}
					});
				});

				throw new FooException();
			});
		}, FooException);
	}

	public function test_catching_awaiting_child() {
		Assert.raises(() -> {
			CoroRun.runScoped(node -> {
				final child = node.async(node -> {
					yield();

					throw new FooException();
				});

				AssertAsync.raises(() -> child.await(), FooException);
			});
		}, FooException);
	}

	public function test_child_throwing_cancelling_parent() {
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			final child = node.async(node -> {
				delay(1000);

				throw new FooException();
			});

			while (true) {
				yield();
			}
		});

		task.start();

		scheduler.advanceBy(1000);

		Assert.isFalse(task.isActive());
		Assert.isOfType(task.getError(), FooException);
	}

	public function test_manually_cancelling_child() {
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			final child = node.async(node -> {
				delay(1000);
			});

			delay(500);

			child.cancel();
		});

		task.start();

		scheduler.advanceBy(500);

		Assert.isFalse(task.isActive());
	}

	public function test_manually_cancelling_polling_child() {
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			final child = node.async(node -> {
				while (true) {
					yield();
				}
			});

			delay(500);

			child.cancel();
		});

		task.start();

		scheduler.advanceBy(500);

		Assert.isFalse(task.isActive());
	}
}