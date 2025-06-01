package issues.aidan;

import structured.TestThrowingScopes;
import haxe.coro.schedulers.VirtualTimeScheduler;
import haxe.Exception;

class Issue113 extends utest.Test {
	function testAwaitSingleChild() {
		var scheduler = new VirtualTimeScheduler();

		final task = CoroRun.with(scheduler).create(node -> {
			final child1 = node.async(_ -> delay(10));
			node.awaitChildren();
		});
		task.start();
		scheduler.advanceTo(9);
		Assert.isTrue(task.isActive());
		scheduler.advanceTo(10);
		Assert.isFalse(task.isActive());
	}

	function testAwaitSingleThrowingChild() {
		var scheduler = new VirtualTimeScheduler();

		final task = CoroRun.with(scheduler).create(node -> {
			final child1 = node.async(_ -> {
				delay(10);
				throw new Exception("thrown");
			});
			node.awaitChildren();
		});
		task.start();
		scheduler.advanceTo(9);
		Assert.isTrue(task.isActive());
		scheduler.advanceTo(10);
		Assert.isFalse(task.isActive());
		Assert.equals("thrown", task.getError().message);
	}

	function testAwaitTwoChildren() {
		var scheduler = new VirtualTimeScheduler();

		final task = CoroRun.with(scheduler).create(node -> {
			final child1 = node.async(_ -> delay(10));
			final child2 = node.async(_ -> delay(20));
			node.awaitChildren();
		});
		task.start();
		scheduler.advanceTo(9);
		Assert.isTrue(task.isActive());
		scheduler.advanceTo(10);
		Assert.isTrue(task.isActive());
		scheduler.advanceTo(20);
		Assert.isFalse(task.isActive());
	}

	function testAwaitLazyChild() {
		var scheduler = new VirtualTimeScheduler();

		final task = CoroRun.with(scheduler).create(node -> {
			final child1 = node.lazy(_ -> delay(10));
			node.awaitChildren();
		});
		task.start();
		scheduler.advanceTo(9);
		Assert.isTrue(task.isActive());
		scheduler.advanceTo(10);
		Assert.isFalse(task.isActive());
	}

	function testAwaitLazyChain() {
		var scheduler = new VirtualTimeScheduler();

		final task = CoroRun.with(scheduler).create(node -> {
			final child1 = node.lazy(_ -> delay(10));
			final child2 = node.lazy(_ -> {
				child1.await();
				delay(10);
			});
			node.awaitChildren();
		});
		task.start();
		scheduler.advanceTo(9);
		Assert.isTrue(task.isActive());
		scheduler.advanceTo(10);
		Assert.isTrue(task.isActive());
		scheduler.advanceTo(15);
		Assert.isTrue(task.isActive());
		scheduler.advanceTo(20);
		Assert.isFalse(task.isActive());
	}

	function testAwaitManyRandomChildren() {
		var scheduler = new VirtualTimeScheduler();

		final task = CoroRun.with(scheduler).create(node -> {
			var k = 0;
			for (_ in 0...1000) {
				node.async(_ -> {
					delay(Std.random(100));
					k++;
				});
			}
			node.awaitChildren();
			k;
		});
		task.start();
		Assert.isTrue(task.isActive());
		scheduler.advanceTo(100);
		Assert.isFalse(task.isActive());
		Assert.equals(1000, task.get());
	}

	function testAwaitManyRandomLazyChildren() {
		var scheduler = new VirtualTimeScheduler();

		final task = CoroRun.with(scheduler).create(node -> {
			var k = 0;
			for (_ in 0...1000) {
				node.lazy(_ -> {
					delay(Std.random(100));
					k++;
				});
			}
			node.awaitChildren();
			k;
		});
		task.start();
		Assert.isTrue(task.isActive());
		scheduler.advanceTo(100);
		Assert.isFalse(task.isActive());
		Assert.equals(1000, task.get());
	}

	function testAwaitManyRandomLazyChildrenAndOneOfThemThrows() {
		var scheduler = new VirtualTimeScheduler();

		final task = CoroRun.with(scheduler).create(node -> {
			var k = 0;
			for (_ in 0...1000) {
				node.lazy(_ -> {
					delay(Std.random(100));
					k++;
					if (k == 1000) {
						throw new Exception('done: $k');
					}
				});
			}
			node.awaitChildren();
			k;
		});
		task.start();
		Assert.isTrue(task.isActive());
		scheduler.advanceTo(100);
		Assert.isFalse(task.isActive());
		Assert.equals("done: 1000", task.getError().message);
	}
}