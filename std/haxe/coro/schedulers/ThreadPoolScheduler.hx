package haxe.coro.schedulers;

#if target.threaded
import sys.thread.Thread;
import sys.thread.IThreadPool;
import haxe.Int64;
import haxe.exceptions.ArgumentException;

private class NoOpHandle implements ISchedulerHandle {
	public function new() {}
	public function close() {}
}

final class ThreadPoolScheduler extends Scheduler {
	final pool : IThreadPool;

	final thread : Thread;

	final eventLoop : EventLoopScheduler;

	public function new(pool) {
		super();

		this.pool      = pool;
		this.eventLoop = new EventLoopScheduler();
		this.thread    = Thread.create(keepAlive);
	}

	public function schedule(ms:Int64, func:()->Void):ISchedulerHandle {
		if (ms < 0) {
			throw new ArgumentException("Time must be greater or equal to zero");
		}

		if (0 == ms) {
			pool.run(func);

			return new NoOpHandle();
		}

		return eventLoop.schedule(ms, pool.run.bind(func));
	}

	public function scheduleObject(obj:IScheduleObject):Void {
		pool.run(obj.onSchedule);
	}

	public function now() {
		return eventLoop.now();
	}

	function keepAlive() {
		while (true) {
			eventLoop.run();
		}
	}
}
#end