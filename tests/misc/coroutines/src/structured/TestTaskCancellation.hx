package structured;

import haxe.Exception;
import haxe.coro.schedulers.VirtualTimeScheduler;
import haxe.coro.cancellation.ICancellationHandle;
import hxcoro.CoroTask;

class TestTaskCancellation extends utest.Test {
	public function test_cancellation_callback() {
		final result    = [];
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			node.context.get(CoroTask.key).onCancellationRequested(() -> {
				result.push(0);
			});

			delay(1000);
		});

		task.start();
		task.cancel();

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([ 0 ], result);
	}

	public function test_closing_cancellation_callback() {
		var handle : ICancellationHandle = null;

		final result    = [];
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			handle = node.context.get(CoroTask.key).onCancellationRequested(() -> {
				result.push(0);
			});

			delay(1000);
		});

		task.start();

		scheduler.advanceBy(1);

		handle.close();

		scheduler.advanceBy(1);

		task.cancel();

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([], result);
	}
}