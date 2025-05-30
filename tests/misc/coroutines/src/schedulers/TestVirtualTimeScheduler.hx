package schedulers;

import haxe.coro.schedulers.VirtualTimeScheduler;
import haxe.exceptions.ArgumentException;

class TestVirtualTimeScheduler extends utest.Test {
	public function test_time_after_advancing_by() {
		final sut = new VirtualTimeScheduler();

		Assert.equals(0f64, sut.now());

		sut.advanceBy(100);
		Assert.equals(0.1f64, sut.now());

		sut.advanceBy(400);
		Assert.equals(0.5f64, sut.now());
	}

	public function test_time_after_advancing_to() {
		final sut = new VirtualTimeScheduler();

		Assert.equals(0f64, sut.now());

		sut.advanceTo(100);
		Assert.equals(0.1f64, sut.now());

		sut.advanceTo(400);
		Assert.equals(0.4f64, sut.now());
	}

	public function test_scheduling_immediate_function() {
		final result = [];
		final sut    = new VirtualTimeScheduler();

		sut.schedule(0, () -> result.push(0));
		sut.run();

		Assert.same([ 0 ], result);
	}

	public function test_scheduling_future_function() {
		final result = [];
		final sut    = new VirtualTimeScheduler();

		sut.schedule(10, () -> result.push(0));
		sut.advanceBy(10);

		Assert.same([ 0 ], result);
	}

	public function test_scheduling_multiple_future_function_same_time() {
		final result = [];
		final sut    = new VirtualTimeScheduler();

		sut.schedule(10, () -> result.push(0));
		sut.schedule(10, () -> result.push(1));
		sut.advanceBy(10);

		Assert.same([ 0, 1 ], result);
	}

	public function test_scheduling_all_functions_up_to_time() {
		final result = [];
		final sut    = new VirtualTimeScheduler();

		sut.schedule(10, () -> result.push(0));
		sut.schedule(20, () -> result.push(1));
		sut.advanceBy(20);

		Assert.same([ 0, 1 ], result);
	}

	public function test_scheduling_functions_at_their_due_time() {
		final result = [];
		final sut    = new VirtualTimeScheduler();

		sut.schedule(10, () -> result.push(sut.now()));
		sut.schedule(20, () -> result.push(sut.now()));
		sut.advanceBy(20);

		Assert.same([ 0.01, 0.02 ], result);
	}

	public function test_scheduling_recursive_immediate_functions() {
		final result = [];
		final sut    = new VirtualTimeScheduler();

		sut.schedule(0, () -> {
			result.push(0);

			sut.schedule(0, () -> {
				result.push(1);

				sut.schedule(0, () -> {
					result.push(2);
				});
				sut.run();
			});
			sut.run();
		});
		sut.run();

		Assert.same([ 0, 1, 2 ], result);
	}

	public function test_scheduling_negative_time() {
		final sut = new VirtualTimeScheduler();

		Assert.raises(() -> sut.schedule(-1, () -> {}), ArgumentException);
	}

	public function test_advancing_by_negative_time() {
		final sut = new VirtualTimeScheduler();

		Assert.raises(() -> sut.advanceBy(-1), ArgumentException);
	}

	public function test_advancing_to_the_past() {
		final sut = new VirtualTimeScheduler();

		sut.advanceTo(1000);

		Assert.raises(() -> sut.advanceTo(500), ArgumentException);
	}
}