package ds.channels;

import haxe.coro.context.Context;
import haxe.coro.IContinuation;
import haxe.Exception;
import haxe.exceptions.CancellationException;
import haxe.exceptions.NotImplementedException;
import hxcoro.ds.channels.unbounded.UnboundedReader;
import hxcoro.ds.Out;
import hxcoro.ds.PagedDeque;
import hxcoro.exceptions.ChannelClosedException;
import haxe.coro.schedulers.VirtualTimeScheduler;

using hxcoro.util.Convenience;

class TestUnboundedReader extends utest.Test {
	function test_try_read_has_data() {
		final out    = new Out();
		final buffer = new PagedDeque();
		final reader = new UnboundedReader(buffer, new PagedDeque(), new Out());

		buffer.push(10);

		Assert.isTrue(reader.tryRead(out));
		Assert.equals(10, out.get());
		Assert.isTrue(buffer.isEmpty());
	}

	function test_try_read_empty() {
		final out    = new Out();
		final buffer = new PagedDeque();
		final reader = new UnboundedReader(buffer, new PagedDeque(), new Out());

		Assert.isFalse(reader.tryRead(out));
		Assert.isTrue(buffer.isEmpty());
	}

	function test_try_peek_has_data() {
		final out    = new Out();
		final buffer = new PagedDeque();
		final reader = new UnboundedReader(buffer, new PagedDeque(), new Out());

		buffer.push(10);

		Assert.isTrue(reader.tryPeek(out));
		Assert.equals(10, out.get());
		if (Assert.isFalse(buffer.isEmpty())) {
			Assert.equals(10, buffer.pop());
		}
	}

	function test_try_peek_many_data() {
		final out    = new Out();
		final buffer = new PagedDeque();
		final reader = new UnboundedReader(buffer, new PagedDeque(), new Out());

		for (i in 0...10) {
			buffer.push(i + 1);
		}

		Assert.isTrue(reader.tryPeek(out));
		Assert.equals(10, out.get());
		Assert.isFalse(buffer.isEmpty());
	}

	function test_try_peek_empty() {
		final reader = new UnboundedReader(new PagedDeque(), new PagedDeque(), new Out());
		final out    = new Out();

		Assert.isFalse(reader.tryPeek(out));
	}

	function test_wait_for_read_has_data() {
		final out     = new Out();
		final buffer  = new PagedDeque();
		final waiters = new PagedDeque();
		final reader  = new UnboundedReader(buffer, waiters, new Out());

		buffer.push(10);

		final scheduler = new VirtualTimeScheduler();
		final actual    = [];
		final task      = CoroRun.with(scheduler).create(node -> {
			actual.push(reader.waitForRead());
		});

		task.start();

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([ true ], actual);
		Assert.isTrue(waiters.isEmpty());
	}

	function test_wait_for_read_empty_buffer() {
		final out       = new Out();
		final buffer    = new PagedDeque();
		final waiters   = new PagedDeque();
		final reader    = new UnboundedReader(buffer, waiters, new Out());
		final scheduler = new VirtualTimeScheduler();
		final actual    = [];
		final task      = CoroRun.with(scheduler).create(node -> {
			actual.push(reader.waitForRead());
		});

		task.start();

		scheduler.advanceBy(1);

		Assert.isTrue(task.isActive());
		Assert.same([], actual);
		Assert.isFalse(waiters.isEmpty());
	}

	function test_wait_for_read_empty_buffer_wakeup() {
		final out       = new Out();
		final buffer    = new PagedDeque();
		final waiters   = new PagedDeque();
		final reader    = new UnboundedReader(buffer, waiters, new Out());
		final scheduler = new VirtualTimeScheduler();
		final actual    = [];
		final task      = CoroRun.with(scheduler).create(node -> {
			actual.push(reader.waitForRead());
		});

		task.start();

		scheduler.advanceBy(1);

		waiters.pop().succeedAsync(true);

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([true], actual);
		Assert.isTrue(waiters.isEmpty());
	}

	function test_wait_for_read_empty_buffer_cancellation() {
		final out       = new Out();
		final buffer    = new PagedDeque();
		final waiters   = new PagedDeque();
		final reader    = new UnboundedReader(buffer, waiters, new Out());
		final scheduler = new VirtualTimeScheduler();
		final actual    = [];
		final task      = CoroRun.with(scheduler).create(node -> {
			actual.push(reader.waitForRead());
		});

		task.start();

		scheduler.advanceBy(1);

		task.cancel();

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.isOfType(task.getError(), CancellationException);
		Assert.same([], actual);
		Assert.isTrue(waiters.isEmpty());
	}

	function test_read_has_data() {
		final out     = new Out();
		final buffer  = new PagedDeque();
		final waiters = new PagedDeque();
		final reader  = new UnboundedReader(buffer, waiters, new Out());

		buffer.push(10);

		final scheduler = new VirtualTimeScheduler();
		final actual    = [];
		final task      = CoroRun.with(scheduler).create(node -> {
			actual.push(reader.read());
		});

		task.start();

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([ 10 ], actual);
		Assert.isTrue(buffer.isEmpty());
		Assert.isTrue(waiters.isEmpty());
	}

	function test_read_empty_buffer() {
		final out       = new Out();
		final buffer    = new PagedDeque();
		final waiters   = new PagedDeque();
		final reader    = new UnboundedReader(buffer, waiters, new Out());
		final scheduler = new VirtualTimeScheduler();
		final actual    = [];
		final task      = CoroRun.with(scheduler).create(node -> {
			actual.push(reader.read());
		});

		task.start();

		scheduler.advanceBy(1);

		Assert.isTrue(task.isActive());
		Assert.same([], actual);
		Assert.isTrue(buffer.isEmpty());
		Assert.isFalse(waiters.isEmpty());
	}

	function test_read_empty_buffer_wakeup() {
		final out       = new Out();
		final buffer    = new PagedDeque();
		final waiters   = new PagedDeque();
		final reader    = new UnboundedReader(buffer, waiters, new Out());
		final scheduler = new VirtualTimeScheduler();
		final actual    = [];
		final task      = CoroRun.with(scheduler).create(node -> {
			actual.push(reader.read());
		});

		task.start();
		
		scheduler.advanceBy(1);

		Assert.isTrue(task.isActive());
		Assert.isTrue(buffer.isEmpty());

		buffer.push(10);
		waiters.pop().succeedAsync(true);

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([ 10 ], actual);
		Assert.isTrue(waiters.isEmpty());
		Assert.isTrue(buffer.isEmpty());
	}

	function test_read_cancellation() {
		final out       = new Out();
		final buffer    = new PagedDeque();
		final waiters   = new PagedDeque();
		final reader    = new UnboundedReader(buffer, waiters, new Out());
		final scheduler = new VirtualTimeScheduler();
		final actual    = [];
		final task      = CoroRun.with(scheduler).create(node -> {
			actual.push(reader.read());
		});

		task.start();
		scheduler.advanceBy(1);
		task.cancel();
		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.isOfType(task.getError(), CancellationException);
		Assert.isTrue(buffer.isEmpty());
		Assert.isTrue(waiters.isEmpty());
	}

	function test_wait_for_Read_when_closed() {
		final closed    = new Out();
		final buffer    = new PagedDeque();
		final waiters   = new PagedDeque();
		final reader    = new UnboundedReader(buffer, waiters, closed);
		final scheduler = new VirtualTimeScheduler();
		final actual    = [];
		final task      = CoroRun.with(scheduler).create(node -> {
			actual.push(reader.waitForRead());
		});

		closed.set(true);

		task.start();
		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([ false ], actual);
	}

	function test_wait_for_read_when_closed_with_remaining_data() {
		final closed    = new Out();
		final buffer    = new PagedDeque();
		final waiters   = new PagedDeque();
		final reader    = new UnboundedReader(buffer, waiters, closed);
		final scheduler = new VirtualTimeScheduler();
		final actual    = [];
		final task      = CoroRun.with(scheduler).create(node -> {
			actual.push(reader.waitForRead());
		});

		buffer.push(10);
		closed.set(true);

		task.start();
		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([ true ], actual);
	}

	function test_try_read_when_closed() {
		final closed  = new Out();
		final buffer  = new PagedDeque();
		final waiters = new PagedDeque();
		final reader  = new UnboundedReader(buffer, waiters, closed);

		closed.set(true);

		Assert.isFalse(reader.tryRead(new Out()));
	}

	function test_try_read_when_closed_with_remaining_data() {
		final closed  = new Out();
		final buffer  = new PagedDeque();
		final waiters = new PagedDeque();
		final reader  = new UnboundedReader(buffer, waiters, closed);
		final out     = new Out();

		buffer.push(10);
		closed.set(true);

		Assert.isTrue(reader.tryRead(out));
		Assert.isTrue(buffer.isEmpty());
		Assert.equals(10, out.get());
	}

	function test_read_when_closed() {
		final closed    = new Out();
		final buffer    = new PagedDeque();
		final waiters   = new PagedDeque();
		final reader    = new UnboundedReader(buffer, waiters, closed);
		final scheduler = new VirtualTimeScheduler();
		final actual    = [];
		final task      = CoroRun.with(scheduler).create(node -> {
			AssertAsync.raises(reader.read(), ChannelClosedException);
		});

		closed.set(true);

		task.start();
		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([], actual);
	}

	function test_read_when_closed_with_remaining_data() {
		final closed    = new Out();
		final buffer    = new PagedDeque();
		final waiters   = new PagedDeque();
		final reader    = new UnboundedReader(buffer, waiters, closed);
		final scheduler = new VirtualTimeScheduler();
		final actual    = [];
		final task      = CoroRun.with(scheduler).create(node -> {
			actual.push(reader.read());
		});

		closed.set(true);
		buffer.push(10);

		task.start();
		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([10], actual);
	}
}