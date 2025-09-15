package ds.channels;

import haxe.coro.context.Context;
import haxe.coro.IContinuation;
import haxe.Exception;
import haxe.exceptions.CancellationException;
import haxe.exceptions.NotImplementedException;
import hxcoro.ds.channels.bounded.BoundedWriter;
import hxcoro.ds.Out;
import hxcoro.ds.CircularBuffer;
import hxcoro.ds.PagedDeque;
import hxcoro.exceptions.ChannelClosedException;
import haxe.coro.schedulers.VirtualTimeScheduler;

using hxcoro.util.Convenience;

private class TestContinuation<T> implements IContinuation<Bool> {
	final expected : Array<T>;
	final mapper : Bool->T;

	public var context (get, never) : Context;

	function get_context():Context {
		return Context.create(new ImmediateScheduler());
	}

	public function new(expected : Array<T>, mapper : Bool->T) {
		this.expected = expected;
		this.mapper   = mapper;
	}

	public function resume(result:Bool, _:Exception) {
		expected.push(mapper(result));
	}
}

class TestBoundedWriter extends utest.Test {
	function test_try_write_has_space() {
		final buffer        = new CircularBuffer(1);
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final writer        = new BoundedWriter(buffer, writeWaiters, readWaiters, new Out(), Wait);

		Assert.isTrue(writer.tryWrite(10));

		final out = new Out();
		if (Assert.isTrue(buffer.tryPeekHead(out))) {
			Assert.equals(10, out.get());
		}
	}

	function test_try_write_full_buffer() {
		final buffer        = new CircularBuffer(1);
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final writer        = new BoundedWriter(buffer, writeWaiters, readWaiters, new Out(), Wait);
		final out           = new Out();

		Assert.isTrue(buffer.tryPush(5));
		Assert.isFalse(writer.tryWrite(10));
		if (Assert.isTrue(buffer.tryPeekHead(out))) {
			Assert.equals(5, out.get());
		}
	}

	function test_try_write_wakeup_all_readers() {
		final buffer        = new CircularBuffer(1);
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final writer        = new BoundedWriter(buffer, writeWaiters, readWaiters, new Out(), Wait);
		final expected      = [];

		readWaiters.push(new TestContinuation(expected, _ -> '1'));
		readWaiters.push(new TestContinuation(expected, _ -> '2'));

		Assert.isTrue(writer.tryWrite(10));
		Assert.isTrue(readWaiters.isEmpty());
		Assert.same([ '1', '2' ], expected);
	}

	function test_wait_for_write_empty_buffer() {
		final buffer        = new CircularBuffer(2);
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final writer        = new BoundedWriter(buffer, writeWaiters, readWaiters, new Out(), Wait);
		final scheduler     = new VirtualTimeScheduler();
		final actual        = [];
		final task          = CoroRun.with(scheduler).create(node -> {
			actual.push(writer.waitForWrite());
		});

		task.start();

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([ true ], actual);
		Assert.isTrue(writeWaiters.isEmpty());
	}

	function test_wait_for_write_partial_buffer() {
		final buffer        = new CircularBuffer(2);
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final writer        = new BoundedWriter(buffer, writeWaiters, readWaiters, new Out(), Wait);
		final scheduler     = new VirtualTimeScheduler();
		final actual        = [];
		final task          = CoroRun.with(scheduler).create(node -> {
			actual.push(writer.waitForWrite());
		});

		Assert.isTrue(buffer.tryPush(0));

		task.start();

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([ true ], actual);
		Assert.isTrue(writeWaiters.isEmpty());
	}

	function test_wait_for_write_full_buffer() {
		final buffer        = new CircularBuffer(2);
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final writer        = new BoundedWriter(buffer, writeWaiters, readWaiters, new Out(), Wait);
		final scheduler     = new VirtualTimeScheduler();
		final actual        = [];
		final task = CoroRun.with(scheduler).create(node -> {
			actual.push(writer.waitForWrite());
		});

		Assert.isTrue(buffer.tryPush(0));
		Assert.isTrue(buffer.tryPush(0));

		task.start();

		scheduler.advanceBy(1);

		Assert.isTrue(task.isActive());
		Assert.same([], actual);
		Assert.isFalse(writeWaiters.isEmpty());
	}

	function test_wait_for_write_full_buffer_wakeup() {
		final buffer        = new CircularBuffer(2);
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final writer        = new BoundedWriter(buffer, writeWaiters, readWaiters, new Out(), Wait);
		final scheduler     = new VirtualTimeScheduler();
		final actual        = [];
		final task          = CoroRun.with(scheduler).create(node -> {
			actual.push(writer.waitForWrite());
		});

		Assert.isTrue(buffer.tryPush(0));
		Assert.isTrue(buffer.tryPush(0));

		task.start();

		scheduler.advanceBy(1);

		writeWaiters.pop().succeedAsync(true);

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([ true ], actual);
		Assert.isTrue(writeWaiters.isEmpty());
	}

	function test_wait_for_write_full_buffer_cancellation() {
		final buffer        = new CircularBuffer(2);
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final writer        = new BoundedWriter(buffer, writeWaiters, readWaiters, new Out(), Wait);
		final scheduler     = new VirtualTimeScheduler();
		final actual        = [];
		final task          = CoroRun.with(scheduler).create(node -> {
			actual.push(writer.waitForWrite());
		});

		Assert.isTrue(buffer.tryPush(0));
		Assert.isTrue(buffer.tryPush(0));

		task.start();

		scheduler.advanceBy(1);

		task.cancel();

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.isOfType(task.getError(), CancellationException);
		Assert.same([], actual);
		Assert.isTrue(writeWaiters.isEmpty());
	}

	function test_write_has_space() {
		final buffer        = new CircularBuffer(1);
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final writer        = new BoundedWriter(buffer, writeWaiters, readWaiters, new Out(), Wait);
		final scheduler     = new VirtualTimeScheduler();
		final out           = new Out();
		final task          = CoroRun.with(scheduler).create(node -> {
			writer.write(10);
		});

		task.start();
		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		if (Assert.isTrue(buffer.tryPeekHead(out))) {
			Assert.equals(10, out.get());
		}
		Assert.isTrue(writeWaiters.isEmpty());
	}

	function test_write_wait_full_buffer() {
		final buffer        = new CircularBuffer(1);
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final writer        = new BoundedWriter(buffer, writeWaiters, readWaiters, new Out(), Wait);
		final scheduler     = new VirtualTimeScheduler();
		final out           = new Out();
		final task          = CoroRun.with(scheduler).create(node -> {
			writer.write(20);
		});

		Assert.isTrue(buffer.tryPush(10));

		task.start();
		scheduler.advanceBy(1);

		Assert.isTrue(task.isActive());
		Assert.isFalse(writeWaiters.isEmpty());
		if (Assert.isTrue(buffer.tryPeekHead(out))) {
			Assert.equals(10, out.get());
		}
	}

	function test_write_drop_write_full_buffer() {
		final buffer        = new CircularBuffer(1);
		final dropped       = [];
		final maxBufferSize = 1;
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final writer        = new BoundedWriter(buffer, writeWaiters, readWaiters, new Out(), DropWrite(v -> dropped.push(v)));
		final scheduler     = new VirtualTimeScheduler();
		final out           = new Out();
		final task          = CoroRun.with(scheduler).create(node -> {
			writer.write(20);
		});

		Assert.isTrue(buffer.tryPush(10));

		task.start();
		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([ 20 ], dropped);
		Assert.isTrue(writeWaiters.isEmpty());
		if (Assert.isTrue(buffer.tryPeekHead(out))) {
			Assert.equals(10, out.get());
		}
	}

	function test_write_drop_newest_full_buffer() {
		final buffer        = new CircularBuffer(3);
		final dropped       = [];
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final writer        = new BoundedWriter(buffer, writeWaiters, readWaiters, new Out(), DropNewest(v -> dropped.push(v)));
		final scheduler     = new VirtualTimeScheduler();
		final out           = new Out();
		final task          = CoroRun.with(scheduler).create(node -> {
			writer.write(20);
		});

		Assert.isTrue(buffer.tryPush(1));
		Assert.isTrue(buffer.tryPush(2));
		Assert.isTrue(buffer.tryPush(3));

		task.start();
		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([ 3 ], dropped);
		Assert.isTrue(writeWaiters.isEmpty());
		if (Assert.isTrue(buffer.tryPeekHead(out))) {
			Assert.equals(20, out.get());
		}
	}

	function test_write_drop_oldest_full_buffer() {
		final buffer        = new CircularBuffer(3);
		final dropped       = [];
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final writer        = new BoundedWriter(buffer, writeWaiters, readWaiters, new Out(), DropOldest(v -> dropped.push(v)));
		final scheduler     = new VirtualTimeScheduler();
		final out           = new Out();
		final task          = CoroRun.with(scheduler).create(node -> {
			writer.write(20);
		});

		Assert.isTrue(buffer.tryPush(1));
		Assert.isTrue(buffer.tryPush(2));
		Assert.isTrue(buffer.tryPush(3));

		task.start();
		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([ 1 ], dropped);
		Assert.isTrue(writeWaiters.isEmpty());
		if (Assert.isTrue(buffer.tryPeekHead(out))) {
			Assert.equals(20, out.get());
		}
	}

	function test_write_wakup_all_readers() {
		final buffer        = new CircularBuffer(1);
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final writer        = new BoundedWriter(buffer, writeWaiters, readWaiters, new Out(), Wait);
		final scheduler     = new VirtualTimeScheduler();
		final expected      = [];
		final task          = CoroRun.with(scheduler).create(node -> {
			writer.write(10);
		});

		readWaiters.push(new TestContinuation(expected, _ -> '1'));
		readWaiters.push(new TestContinuation(expected, _ -> '2'));

		task.start();
		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.isTrue(readWaiters.isEmpty());
		Assert.same([ '1', '2' ], expected);
	}

	function test_write_full_buffer_wakeup() {
		final buffer        = new CircularBuffer(1);
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final writer        = new BoundedWriter(buffer, writeWaiters, readWaiters, new Out(), Wait);
		final scheduler     = new VirtualTimeScheduler();
		final task          = CoroRun.with(scheduler).create(node -> {
			writer.write(10);
		});

		Assert.isTrue(buffer.tryPush(5));

		task.start();
		scheduler.advanceBy(1);

		Assert.isTrue(task.isActive());

		final out = new Out();
		if (Assert.isTrue(buffer.tryPeekHead(out))) {
			Assert.equals(5, out.get());
		}

		Assert.isTrue(buffer.tryPopHead(out));
		Assert.isTrue(buffer.wasEmpty());
		writeWaiters.pop().succeedAsync(true);

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		if (Assert.isTrue(buffer.tryPeekHead(out))) {
			Assert.equals(10, out.get());
		}
	}

	function test_write_cancellation() {
		final buffer        = new CircularBuffer(1);
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final writer        = new BoundedWriter(buffer, writeWaiters, readWaiters, new Out(), Wait);
		final scheduler     = new VirtualTimeScheduler();
		final out           = new Out();
		final task          = CoroRun.with(scheduler).create(node -> {
			writer.write(10);
		});

		Assert.isTrue(buffer.tryPush(5));

		task.start();
		scheduler.advanceBy(1);
		task.cancel();
		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.isOfType(task.getError(), CancellationException);
		Assert.isTrue(writeWaiters.isEmpty());
		if (Assert.isTrue(buffer.tryPeekHead(out))) {
			Assert.equals(5, out.get());
		}
	}

	function test_close_sets_out() {
		final buffer        = new CircularBuffer(1);
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final closed        = new Out();
		final writer        = new BoundedWriter(buffer, writeWaiters, readWaiters, closed, Wait);

		closed.set(false);
		writer.close();

		Assert.isTrue(closed.get());
	}

	function test_try_write_when_closed() {
		final buffer        = new CircularBuffer(1);
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final writer        = new BoundedWriter(buffer, writeWaiters, readWaiters, new Out(), Wait);

		writer.close();

		Assert.isFalse(writer.tryWrite(10));
		Assert.isTrue(buffer.wasEmpty());
	}

	function test_wait_for_write_when_closed() {
		final buffer        = new CircularBuffer(1);
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final writer        = new BoundedWriter(buffer, writeWaiters, readWaiters, new Out(), Wait);
		final scheduler     = new VirtualTimeScheduler();
		final actual        = [];
		final task          = CoroRun.with(scheduler).create(node -> {
			actual.push(writer.waitForWrite());
		});

		writer.close();

		task.start();
		scheduler.advanceBy(1);

		Assert.same([ false ], actual);
	}

	function test_write_when_closed() {
		final buffer        = new CircularBuffer(1);
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final writer        = new BoundedWriter(buffer, writeWaiters, readWaiters, new Out(), Wait);
		final scheduler     = new VirtualTimeScheduler();
		final actual        = [];
		final task          = CoroRun.with(scheduler).create(node -> {
			AssertAsync.raises(() -> writer.write('hello'), ChannelClosedException);
		});

		writer.close();

		task.start();
		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
	}

	function test_closing_wakesup_write_waiters() {
		final buffer        = new CircularBuffer(1);
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final writer        = new BoundedWriter(buffer, writeWaiters, readWaiters, new Out(), Wait);
		final scheduler     = new VirtualTimeScheduler();
		final actual        = [];
		final task          = CoroRun.with(scheduler).create(node -> {
			actual.push(writer.waitForWrite());
		});

		Assert.isTrue(buffer.tryPush(0));

		task.start();

		scheduler.advanceBy(1);
		Assert.same([], actual);

		writer.close();

		scheduler.advanceBy(1);
		Assert.same([ false ], actual);
	}

	function test_closing_wakesup_read_waiters() {
		final buffer        = new CircularBuffer(1);
		final writeWaiters  = new PagedDeque();
		final readWaiters   = new PagedDeque();
		final writer        = new BoundedWriter(buffer, writeWaiters, readWaiters, new Out(), Wait);
		final scheduler     = new VirtualTimeScheduler();
		final actual        = [];
		final task          = CoroRun.with(scheduler).create(node -> {
			writer.waitForWrite();
		});

		readWaiters.push(new TestContinuation(actual, b -> b));

		Assert.isTrue(buffer.tryPush(0));

		task.start();

		scheduler.advanceBy(1);
		Assert.same([], actual);

		writer.close();

		scheduler.advanceBy(1);
		Assert.same([ false ], actual);
	}
}