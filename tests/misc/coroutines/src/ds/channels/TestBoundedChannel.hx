package ds.channels;

import haxe.ds.Option;
import haxe.coro.schedulers.VirtualTimeScheduler;
import haxe.exceptions.ArgumentException;
import hxcoro.ds.Out;
import hxcoro.ds.channels.Channel;
import hxcoro.exceptions.TimeoutException;

class TestBoundedChannel extends utest.Test {
	public function test_creating() {
		Assert.notNull(Channel.createBounded({ size : 3 }));
	}

	public function test_invalid_size() {
		Assert.raises(() -> Channel.createBounded({ size : 0 }), ArgumentException);
	}

	public function test_general() {
		final size = 100;
		final channel = Channel.createBounded({ size : 3 });
		final scheduler = new VirtualTimeScheduler();
		final task = CoroRun.with(scheduler).create(node -> {
			final output = [];
			final writer = node.async(_ -> {
				var i = size;

				while (i >= 0) {
					channel.writer.write(i);

					i--;

					delay(Std.random(5));
				}
			});
			for (_ in 0...size + 1) {
				output.push(channel.reader.read());
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

	public function test_fifo_buffered_read_writes() {
		final actual    = [];
		final channel   = Channel.createBounded({ size : 2 });
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			channel.writer.write('Hello');
			channel.writer.write('World');

			actual.push(channel.reader.read());
			actual.push(channel.reader.read());
		});

		task.start();

		scheduler.advanceBy(1);
		
		Assert.isFalse(task.isActive());
		Assert.same([ 'Hello', 'World' ], actual);
	}

	public function test_fifo_suspended_read_writes() {
		final actual    = [];
		final channel   = Channel.createBounded({ size : 1 });
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			channel.writer.write('dummy');

			node.async(_ -> {
				channel.writer.write('Hello');
			});

			node.async(_ -> {
				channel.writer.write('World');
			});

			delay(100);

			actual.push(channel.reader.read());
			actual.push(channel.reader.read());
			actual.push(channel.reader.read());
		});

		task.start();

		scheduler.advanceBy(100);
		
		Assert.isFalse(task.isActive());
		Assert.same([ 'dummy', 'Hello', 'World' ], actual);
	}

		function test_write_cancellation() {
		final actual     = [];
		final exceptions = [];
		final channel    = Channel.createBounded({ size : 1 });
		final scheduler  = new VirtualTimeScheduler();
		final task       = CoroRun.with(scheduler).create(node -> {
			channel.writer.write('dummy');

			node.async(_ -> {
				try {
					timeout(100, _ -> {
						channel.writer.write('Hello');
					});
				} catch (_:TimeoutException) {
					exceptions.push(scheduler.now());
				}
			});

			node.async(_ -> {
				channel.writer.write('World');
			});

			delay(200);

			Assert.equals('dummy', channel.reader.read());
			
			actual.push(channel.reader.read());
		});

		task.start();

		scheduler.advanceBy(99);
		Assert.same([], actual);

		scheduler.advanceBy(1);
		Assert.same([], actual);
		Assert.equals(1, exceptions.length);
		Assert.isTrue(100i64 == exceptions[0]);

		scheduler.advanceBy(100);
		Assert.same([ 'World' ], actual);

		Assert.isFalse(task.isActive());
	}

	function test_read_cancellation() {
		final actual     = [];
		final exceptions = [];
		final channel    = Channel.createBounded({ size : 1 });
		final scheduler  = new VirtualTimeScheduler();
		final task       = CoroRun.with(scheduler).create(node -> {
			node.async(_ -> {
				try {
					timeout(100, _ -> {
						return channel.reader.read();
					});
				} catch(_:TimeoutException) {
					exceptions.push(scheduler.now());
					"";
				}
			});

			node.async(_ -> {
				actual.push(channel.reader.read());
			});

			delay(200);

			channel.writer.write('Hello');
		});

		task.start();

		scheduler.advanceBy(100);

		Assert.isTrue(task.isActive());
		Assert.same([], actual);
		Assert.equals(1, exceptions.length);
		Assert.isTrue(100i64 == exceptions[0]);

		scheduler.advanceBy(100);

		Assert.isFalse(task.isActive());
		Assert.same([ 'Hello' ], actual);
	}

		function test_try_read() {
		final channel = Channel.createBounded({ size : 1 });
		final scheduler = new VirtualTimeScheduler();
		final task = CoroRun.with(scheduler).create(node -> {
			final output = [];
			node.async(node -> {
				var out = new Out();
				function report(didRead:Bool) {
					if (didRead) {
						output.push(Some(out.get()));
					} else {
						output.push(None);
					}
				}
				// from buffer
				report(channel.reader.tryRead(out));
				delay(2);
				report(channel.reader.tryRead(out));
				report(channel.reader.tryRead(out));

				// from suspense
				delay(2);
				report(channel.reader.tryRead(out));
				yield();
				report(channel.reader.tryRead(out));
				yield();
				report(channel.reader.tryRead(out));
			});
			delay(1);
			channel.writer.write(1);
			delay(2);
			channel.writer.write(2);
			channel.writer.write(3);
			output;
		});
		task.start();
		while (task.isActive()) {
			scheduler.run();
			scheduler.advanceBy(1);
		}
		Assert.same([None, Some(1), None, Some(2), Some(3), None], task.get());
	}

	function test_single_writer_multiple_reader() {
		final channel  = Channel.createBounded({ size : 3 });
		final expected = [ for (i in 0...100) i ];
		final actual   = [];

		CoroRun.runScoped(node -> {
			node.async(_ -> {
				for (v in expected) {
					channel.writer.write(v);
				}

				channel.writer.close();
			});

			for (_ in 0...5) {
				node.async(_ -> {
					final out = new Out();

					while (channel.reader.waitForRead()) {
						if (channel.reader.tryRead(out)) {
							actual.push(out.get());
						}
					}
				});
			}
		});

		Assert.same(expected, actual);
	}

	// var todoHoisting = 0;

	// function test_iterator() {
	// 	final size = 50;
	// 	for (bufferSize in [1, 25, 50]) {
	// 		todoHoisting = 0;
	// 		final channel = Channel.createBounded(bufferSize);
	// 		final scheduler = new VirtualTimeScheduler();
	// 		final task = CoroRun.with(scheduler).create(node -> {
	// 			for (i in 0...size) {
	// 				node.async(_ -> channel.writer.write(todoHoisting++));
	// 			}
	// 			delay(1);
	// 			final res = [for (i in channel) i];
	// 			res;
	// 		});
	// 		task.start();
	// 		while (task.isActive()) {
	// 			scheduler.run();
	// 			scheduler.advanceBy(1);
	// 		}
	// 		Assert.same([for (i in 0...size) i], task.get());
	// 	}
	// }
}