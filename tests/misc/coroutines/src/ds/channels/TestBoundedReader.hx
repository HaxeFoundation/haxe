package ds.channels;

import haxe.Exception;
import haxe.coro.IContinuation;
import haxe.coro.context.Context;
import haxe.coro.schedulers.VirtualTimeScheduler;
import haxe.exceptions.ArgumentException;
import haxe.exceptions.CancellationException;
import haxe.exceptions.NotImplementedException;
import hxcoro.exceptions.ChannelClosedException;
import hxcoro.ds.Out;
import hxcoro.ds.PagedDeque;
import hxcoro.ds.CircularBuffer;
import hxcoro.ds.channels.bounded.BoundedReader;

using hxcoro.util.Convenience;

private class TestContinuation<T> implements IContinuation<Bool> {
	final actual : Array<T>;
	final value : T;

	public var context (get, never) : Context;

	function get_context():Context {
		return Context.create(new ImmediateScheduler());
	}

	public function new(actual : Array<T>, value : T) {
		this.actual = actual;
		this.value  = value;
	}

	public function resume(_:Bool, _:Exception) {
		actual.push(value);
	}
}

class TestBoundedReader extends utest.Test {
	function test_try_read_has_data() {
		final buffer        = new CircularBuffer(1);
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final reader        = new BoundedReader(buffer, writeWaiters, readWaiters, new Out());
		final out           = new Out();

		Assert.isTrue(buffer.tryPush(10));
		Assert.isTrue(reader.tryRead(out));
		Assert.equals(10, out.get());
		Assert.isTrue(buffer.wasEmpty());
	}

	function test_try_read_empty() {
		final buffer        = new CircularBuffer(1);
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final reader        = new BoundedReader(buffer, writeWaiters, readWaiters, new Out());
		final out           = new Out();

		Assert.isFalse(reader.tryRead(out));
		Assert.isTrue(buffer.wasEmpty());
	}

	function test_try_read_wakup_all_writers() {
		final buffer        = new CircularBuffer(1);
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final reader        = new BoundedReader(buffer, writeWaiters, readWaiters, new Out());
		final out           = new Out();
		final actual        = [];

		writeWaiters.push(new TestContinuation(actual, '1'));
		writeWaiters.push(new TestContinuation(actual, '2'));

		Assert.isTrue(buffer.tryPush(0));
		Assert.isTrue(reader.tryRead(out));
		Assert.isTrue(writeWaiters.isEmpty());
		Assert.same([ '1', '2' ], actual);
	}

	function test_try_peek_has_data() {
		final buffer = new CircularBuffer(1);
		final reader = new BoundedReader(buffer, new PagedDeque(), new PagedDeque(), new Out());
		
		Assert.isTrue(buffer.tryPush(10));
		
		final out = new Out();
		if (Assert.isTrue(reader.tryPeek(out))) {
			Assert.equals(10, out.get());
		}
		
		Assert.isFalse(buffer.wasEmpty());
	}

	function test_try_peek_many_data() {
		final count  = 5;
		final buffer = new CircularBuffer(count);
		final reader = new BoundedReader(buffer, new PagedDeque(), new PagedDeque(), new Out());
		final out    = new Out();

		for (i in 0...count) {
			Assert.isTrue(buffer.tryPush(i + 1));
		}

		Assert.isTrue(reader.tryPeek(out));
		Assert.equals(count, out.get());
	}

	function test_try_peek_empty() {
		final buffer = new CircularBuffer(1);
		final reader = new BoundedReader(buffer, new PagedDeque(), new PagedDeque(), new Out());
		final out    = new Out();

		Assert.isFalse(reader.tryPeek(out));
	}

	function test_wait_for_read_has_data() {
		final buffer       = new CircularBuffer(1);
		final writeWaiters = new PagedDeque();
		final readWaiters  = new PagedDeque();
		final reader       = new BoundedReader(buffer, writeWaiters, readWaiters, new Out());
		final scheduler    = new VirtualTimeScheduler();
		final actual       = [];
		final task         = CoroRun.with(scheduler).create(node -> {
			actual.push(reader.waitForRead());
		});

		Assert.isTrue(buffer.tryPush(10));

		task.start();

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([ true ], actual);
		Assert.isTrue(readWaiters.isEmpty());
	}

	function test_wait_for_read_empty_buffer() {
		final buffer       = new CircularBuffer(1);
		final writeWaiters = new PagedDeque();
		final readWaiters  = new PagedDeque();
		final reader       = new BoundedReader(buffer, writeWaiters, readWaiters, new Out());
		final out          = new Out();
		final scheduler    = new VirtualTimeScheduler();
		final actual       = [];
		final task         = CoroRun.with(scheduler).create(node -> {
			actual.push(reader.waitForRead());
		});

		task.start();

		scheduler.advanceBy(1);

		Assert.isTrue(task.isActive());
		Assert.same([], actual);
		Assert.isTrue(buffer.wasEmpty());
		Assert.isFalse(readWaiters.isEmpty());
	}

	function test_wait_for_read_empty_buffer_wakeup() {
		final buffer       = new CircularBuffer(1);
		final writeWaiters = new PagedDeque();
		final readWaiters  = new PagedDeque();
		final reader       = new BoundedReader(buffer, writeWaiters, readWaiters, new Out());
		final out          = new Out();
		final scheduler    = new VirtualTimeScheduler();
		final actual       = [];
		final task         = CoroRun.with(scheduler).create(node -> {
			actual.push(reader.waitForRead());
		});

		task.start();

		scheduler.advanceBy(1);

		readWaiters.pop().succeedAsync(true);

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([ true ], actual);
		Assert.isTrue(readWaiters.isEmpty());
	}

	function test_wait_for_write_empty_buffer_cancellation() {
		final buffer       = new CircularBuffer(1);
		final writeWaiters = new PagedDeque();
		final readWaiters  = new PagedDeque();
		final reader       = new BoundedReader(buffer, writeWaiters, readWaiters, new Out());
		final out          = new Out();
		final scheduler    = new VirtualTimeScheduler();
		final actual       = [];
		final task         = CoroRun.with(scheduler).create(node -> {
			actual.push(reader.waitForRead());
		});

		task.start();

		scheduler.advanceBy(1);

		task.cancel();

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.isOfType(task.getError(), CancellationException);
		Assert.same([], actual);
		Assert.isTrue(readWaiters.isEmpty());
	}

	function test_read_has_data() {
		final buffer       = new CircularBuffer(1);
		final writeWaiters = new PagedDeque();
		final readWaiters  = new PagedDeque();
		final reader       = new BoundedReader(buffer, writeWaiters, readWaiters, new Out());
		final out          = new Out();
		final scheduler    = new VirtualTimeScheduler();
		final actual       = [];
		final task         = CoroRun.with(scheduler).create(node -> {
			actual.push(reader.read());
		});

		Assert.isTrue(buffer.tryPush(10));

		task.start();

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([ 10 ], actual);
		Assert.isTrue(buffer.wasEmpty());
		Assert.isTrue(readWaiters.isEmpty());
	}

	function test_read_empty_buffer() {
		final buffer       = new CircularBuffer(1);
		final writeWaiters = new PagedDeque();
		final readWaiters  = new PagedDeque();
		final reader       = new BoundedReader(buffer, writeWaiters, readWaiters, new Out());
		final out          = new Out();
		final scheduler    = new VirtualTimeScheduler();
		final actual       = [];
		final task         = CoroRun.with(scheduler).create(node -> {
			actual.push(reader.read());
		});

		task.start();

		scheduler.advanceBy(1);

		Assert.isTrue(task.isActive());
		Assert.isTrue(buffer.wasEmpty());
		Assert.same([], actual);
		Assert.isFalse(readWaiters.isEmpty());
	}

	function test_read_wakeup_all_writers() {
		final buffer       = new CircularBuffer(1);
		final writeWaiters = new PagedDeque();
		final readWaiters  = new PagedDeque();
		final reader       = new BoundedReader(buffer, writeWaiters, readWaiters, new Out());
		final out          = new Out();
		final scheduler    = new VirtualTimeScheduler();
		final actual       = [];
		final task         = CoroRun.with(scheduler).create(node -> {
			reader.read();
		});

		Assert.isTrue(buffer.tryPush(10));

		writeWaiters.push(new TestContinuation(actual, '1'));
		writeWaiters.push(new TestContinuation(actual, '2'));

		task.start();

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.isTrue(writeWaiters.isEmpty());
		Assert.same([ '1', '2' ], actual);
	}

	function test_read_empty_buffer_wakeup() {
		final buffer       = new CircularBuffer(1);
		final writeWaiters = new PagedDeque();
		final readWaiters  = new PagedDeque();
		final reader       = new BoundedReader(buffer, writeWaiters, readWaiters, new Out());
		final out          = new Out();
		final scheduler    = new VirtualTimeScheduler();
		final actual       = [];
		final task         = CoroRun.with(scheduler).create(node -> {
			actual.push(reader.read());
		});

		task.start();

		scheduler.advanceBy(1);

		Assert.isTrue(task.isActive());
		Assert.isTrue(buffer.wasEmpty());

		Assert.isTrue(buffer.tryPush(10));
		readWaiters.pop().succeedAsync(true);

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([ 10 ], actual);
		Assert.isTrue(buffer.wasEmpty());
	}

	function test_read_cancellation() {
		final buffer       = new CircularBuffer(1);
		final writeWaiters = new PagedDeque();
		final readWaiters  = new PagedDeque();
		final reader       = new BoundedReader(buffer, writeWaiters, readWaiters, new Out());
		final out          = new Out();
		final scheduler    = new VirtualTimeScheduler();
		final actual       = [];
		final task         = CoroRun.with(scheduler).create(node -> {
			actual.push(reader.read());
		});

		task.start();
		scheduler.advanceBy(1);
		task.cancel();
		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.isOfType(task.getError(), CancellationException);
		Assert.isTrue(buffer.wasEmpty());
		Assert.isTrue(readWaiters.isEmpty());
	}

	function test_wait_for_read_when_closed() {
		final buffer       = new CircularBuffer(1);
		final writeWaiters = new PagedDeque();
		final readWaiters  = new PagedDeque();
		final closed       = new Out();
		final reader       = new BoundedReader(buffer, writeWaiters, readWaiters, closed);
		final actual       = [];
		final scheduler    = new VirtualTimeScheduler();
		final task         = CoroRun.with(scheduler).create(node -> {
			actual.push(reader.waitForRead());
		});

		closed.set(true);

		task.start();
		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([ false ], actual);
	}

	function test_wait_for_read_when_closed_with_remaining_data() {
		final buffer       = new CircularBuffer(1);
		final writeWaiters = new PagedDeque();
		final readWaiters  = new PagedDeque();
		final closed       = new Out();
		final reader       = new BoundedReader(buffer, writeWaiters, readWaiters, closed);
		final scheduler    = new VirtualTimeScheduler();
		final actual       = [];
		final task         = CoroRun.with(scheduler).create(node -> {
			actual.push(reader.waitForRead());
		});

		Assert.isTrue(buffer.tryPush(10));

		closed.set(true);

		task.start();
		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([ true ], actual);
	}

	function test_try_read_when_closed() {
		final buffer       = new CircularBuffer(1);
		final writeWaiters = new PagedDeque();
		final readWaiters  = new PagedDeque();
		final closed       = new Out();
		final out          = new Out();
		final reader       = new BoundedReader(buffer, writeWaiters, readWaiters, closed);

		closed.set(true);

		Assert.isFalse(reader.tryRead(out));
	}

	function test_try_read_when_closed_with_remaining_data() {
		final buffer       = new CircularBuffer(1);
		final writeWaiters = new PagedDeque();
		final readWaiters  = new PagedDeque();
		final closed       = new Out();
		final out          = new Out();
		final reader       = new BoundedReader(buffer, writeWaiters, readWaiters, closed);

		Assert.isTrue(buffer.tryPush(10));

		closed.set(true);

		Assert.isTrue(reader.tryRead(out));
		Assert.isTrue(buffer.wasEmpty());
		Assert.equals(10, out.get());
	}

	function test_read_when_closed() {
		final buffer       = new CircularBuffer(1);
		final writeWaiters = new PagedDeque();
		final readWaiters  = new PagedDeque();
		final closed       = new Out();
		final reader       = new BoundedReader(buffer, writeWaiters, readWaiters, closed);
		final actual       = [];
		final scheduler    = new VirtualTimeScheduler();
		final task         = CoroRun.with(scheduler).create(node -> {
			AssertAsync.raises(reader.read(), ChannelClosedException);
		});

		closed.set(true);

		task.start();
		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([], actual);
	}

	function test_read_when_closed_with_remaining_data() {
		final buffer       = new CircularBuffer(1);
		final writeWaiters = new PagedDeque();
		final readWaiters  = new PagedDeque();
		final closed       = new Out();
		final reader       = new BoundedReader(buffer, writeWaiters, readWaiters, closed);
		final actual       = [];
		final scheduler    = new VirtualTimeScheduler();
		final task         = CoroRun.with(scheduler).create(node -> {
			actual.push(reader.read());
		});

		Assert.isTrue(buffer.tryPush(10));

		closed.set(true);

		task.start();
		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([ 10 ], actual);
	}
}