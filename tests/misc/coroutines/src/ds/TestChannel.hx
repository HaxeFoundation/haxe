package ds;

import haxe.coro.schedulers.VirtualTimeScheduler;
import hxcoro.Coro.*;
import hxcoro.CoroRun;
import hxcoro.ds.Channel;
import hxcoro.exceptions.TimeoutException;

class TestChannel extends utest.Test {
	function test() {
		final size = 100;
		final channel = new Channel(3);
		final scheduler = new VirtualTimeScheduler();
		final task = CoroRun.with(scheduler).create(node -> {
			final output = [];
			final writer = node.async(_ -> {
				var i = size;

				while (i >= 0) {
					channel.write(i);

					i--;

					delay(Std.random(5));
				}
			});
			for (_ in 0...size + 1) {
				output.push(channel.read());
				delay(Std.random(5));
			}
			writer.cancel();
			output;
		});
		task.start();
		while (task.isActive()) {
			scheduler.run();
			scheduler.advanceBy(1);
		}
		final expected = [for (i in 0...size + 1) i];
		expected.reverse();
		Assert.same(expected, task.get());
	}

	function test_fifo_writes() {
		final actual    = [];
		final channel   = new Channel(0);
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			node.async(_ -> {
				channel.write('Hello');
			});

			node.async(_ -> {
				channel.write('World');
			});

			delay(100);

			actual.push(channel.read());
			actual.push(channel.read());
		});

		task.start();

		scheduler.advanceBy(100);
		Assert.same([ 'Hello', 'World' ], actual);

		Assert.isFalse(task.isActive());
	}

	function test_fifo_reads() {
		final actual    = [];
		final channel   = new Channel(0);
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			node.async(_ -> {
				actual.push(channel.read());
				actual.push(channel.read());
			});

			delay(100);

			channel.write('Hello');
			channel.write('World');
		});

		task.start();

		scheduler.advanceBy(100);
		Assert.same([ 'Hello', 'World' ], actual);

		Assert.isFalse(task.isActive());
	}

	function test_write_cancellation() {
		final actual     = [];
		final exceptions = [];
		final channel    = new Channel(0);
		final scheduler  = new VirtualTimeScheduler();
		final task       = CoroRun.with(scheduler).create(node -> {
			node.async(_ -> {
				try {
					timeout(100, _ -> {
						channel.write('Hello');
					});
				} catch (_:TimeoutException) {
					exceptions.push(scheduler.nowMs());
				}
			});

			node.async(_ -> {
				channel.write('World');
			});

			delay(200);

			actual.push(channel.read());
		});

		task.start();

		scheduler.advanceBy(99);
		Assert.same([], actual);

		scheduler.advanceBy(1);
		Assert.same([], actual);
		Assert.same([100], exceptions);

		scheduler.advanceBy(100);
		Assert.same([ 'World' ], actual);

		Assert.isFalse(task.isActive());
	}

	function test_read_cancellation() {
		final actual     = [];
		final exceptions = [];
		final channel    = new Channel(0);
		final scheduler  = new VirtualTimeScheduler();
		final task       = CoroRun.with(scheduler).create(node -> {
			node.async(_ -> {
				try {
					timeout(100, _ -> {
						return channel.read();
					});
				} catch(_:TimeoutException) {
					exceptions.push(scheduler.nowMs());
					"";
				}
			});

			node.async(_ -> {
				actual.push(channel.read());
			});

			delay(200);

			channel.write('Hello');
		});

		task.start();

		scheduler.advanceBy(100);
		scheduler.advanceBy(100);

		Assert.same([ 'Hello' ], actual);
		Assert.same([100], exceptions);
		Assert.isFalse(task.isActive());
	}
}