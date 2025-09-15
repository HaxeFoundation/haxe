package ds.channels;

import haxe.coro.context.Context;
import haxe.coro.IContinuation;
import haxe.Exception;
import haxe.exceptions.CancellationException;
import haxe.exceptions.NotImplementedException;
import hxcoro.ds.channels.unbounded.UnboundedWriter;
import hxcoro.ds.Out;
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

class TestUnboundedWriter extends utest.Test {
	function test_try_write() {
		final out    = new Out();
		final buffer = new PagedDeque();
		final writer = new UnboundedWriter(buffer, new PagedDeque(), new Out());

		Assert.isTrue(writer.tryWrite(1));
		Assert.isTrue(writer.tryWrite(2));
		Assert.isTrue(writer.tryWrite(3));

		Assert.isFalse(buffer.isEmpty());

		Assert.isTrue(buffer.tryPop(out));
		Assert.equals(1, out.get());
		Assert.isTrue(buffer.tryPop(out));
		Assert.equals(2, out.get());
		Assert.isTrue(buffer.tryPop(out));
		Assert.equals(3, out.get());
	}

	function test_try_write_wakeup_all_readers() {
		final buffer      = new PagedDeque();
		final readWaiters = new PagedDeque();
		final writer      = new UnboundedWriter(buffer, readWaiters, new Out());
		final expected    = [];

		readWaiters.push(new TestContinuation(expected, _ -> '1'));
		readWaiters.push(new TestContinuation(expected, _ -> '2'));

		Assert.isTrue(writer.tryWrite(10));
		Assert.isTrue(readWaiters.isEmpty());
		Assert.same([ '1', '2' ], expected);
	}

	function test_try_write_when_closed() {
		final out    = new Out();
		final buffer = new PagedDeque();
		final writer = new UnboundedWriter(buffer, new PagedDeque(), out);

		writer.close();

		Assert.isFalse(writer.tryWrite(1));
		Assert.isTrue(buffer.isEmpty());
	}

	function test_wait_for_write() {
		final buffer      = new PagedDeque();
		final readWaiters = new PagedDeque();
		final writer      = new UnboundedWriter(buffer, readWaiters, new Out());
		final scheduler   = new VirtualTimeScheduler();
		final actual      = [];
		final task        = CoroRun.with(scheduler).create(node -> {
			actual.push(writer.waitForWrite());

			buffer.push(0);

			actual.push(writer.waitForWrite());
		});

		task.start();

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([ true, true ], actual);
	}

	function test_wait_for_write_prompt_cancellation() {
		final buffer      = new PagedDeque();
		final readWaiters = new PagedDeque();
		final writer      = new UnboundedWriter(buffer, readWaiters, new Out());
		final scheduler   = new VirtualTimeScheduler();
		final actual      = [];
		final task        = CoroRun.with(scheduler).create(node -> {
			actual.push(writer.waitForWrite());
		});

		task.start();
		task.cancel();

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([], actual);
		Assert.isOfType(task.getError(), CancellationException);
	}

	function test_wait_for_write_when_closed() {
		final buffer      = new PagedDeque();
		final readWaiters = new PagedDeque();
		final writer      = new UnboundedWriter(buffer, readWaiters, new Out());
		final scheduler   = new VirtualTimeScheduler();
		final actual      = [];
		final task        = CoroRun.with(scheduler).create(node -> {
			actual.push(writer.waitForWrite());
		});

		writer.close();
		task.start();

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.same([ false ], actual);
	}

	function test_write() {
		final out       = new Out();
		final buffer    = new PagedDeque();
		final writer    = new UnboundedWriter(buffer, new PagedDeque(), new Out());
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			writer.write(1);
			writer.write(2);
			writer.write(3);
		});

		task.start();

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.isFalse(buffer.isEmpty());

		Assert.isTrue(buffer.tryPop(out));
		Assert.equals(1, out.get());
		Assert.isTrue(buffer.tryPop(out));
		Assert.equals(2, out.get());
		Assert.isTrue(buffer.tryPop(out));
		Assert.equals(3, out.get());
	}

	function test_write_wakup_all_readers() {
		final out         = new Out();
		final buffer      = new PagedDeque();
		final readWaiters = new PagedDeque();
		final writer      = new UnboundedWriter(buffer, readWaiters, new Out());
		final scheduler   = new VirtualTimeScheduler();
		final expected    = [];
		final task        = CoroRun.with(scheduler).create(node -> {
			writer.write(1);
		});

		readWaiters.push(new TestContinuation(expected, _ -> '1'));
		readWaiters.push(new TestContinuation(expected, _ -> '2'));

		task.start();

		task.start();
		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.isTrue(readWaiters.isEmpty());
		Assert.same([ '1', '2' ], expected);
	}

	function test_write_prompt_cancellation() {
		final out       = new Out();
		final buffer    = new PagedDeque();
		final writer    = new UnboundedWriter(buffer, new PagedDeque(), new Out());
		final scheduler = new VirtualTimeScheduler();
		final task      = CoroRun.with(scheduler).create(node -> {
			writer.write(1);
		});

		task.start();
		task.cancel();

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.isTrue(buffer.isEmpty());
		Assert.isOfType(task.getError(), CancellationException);
	}

	function test_write_when_closed() {
		final buffer      = new PagedDeque();
		final readWaiters = new PagedDeque();
		final writer      = new UnboundedWriter(buffer, readWaiters, new Out());
		final scheduler   = new VirtualTimeScheduler();
		final task        = CoroRun.with(scheduler).create(node -> {
			writer.write(0);
		});

		writer.close();
		task.start();

		scheduler.advanceBy(1);

		Assert.isFalse(task.isActive());
		Assert.isTrue(buffer.isEmpty());
		Assert.isOfType(task.getError(), ChannelClosedException);
	}

	function test_close_sets_out() {
		final buffer      = new PagedDeque();
		final closed      = new Out();
		final writer      = new UnboundedWriter(buffer, new PagedDeque(), closed);

		closed.set(false);
		writer.close();

		Assert.isTrue(closed.get());
	}

	function test_closing_wakesup_read_waiters() {
		final buffer      = new PagedDeque();
		final readWaiters = new PagedDeque();
		final writer      = new UnboundedWriter(buffer, readWaiters, new Out());
		final scheduler   = new VirtualTimeScheduler();
		final actual      = [];
		final task        = CoroRun.with(scheduler).create(node -> {
			yield();
		});

		readWaiters.push(new TestContinuation(actual, b -> b));

		writer.close();

		task.start();

		scheduler.advanceBy(1);

		Assert.same([ false ], actual);
	}
}