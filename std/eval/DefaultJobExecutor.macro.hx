package eval;

import haxe.Exception;
import haxe.Callback;
import haxe.NoData;
import haxe.IJobExecutor;
import sys.thread.Thread;

/**
	Default implementation of `haxe.IJobExecutor` for macro target.
**/
class DefaultJobExecutor implements IJobExecutor {
	var active = true;
	final tasks = new Array<Task<Dynamic>>();
	final maxWorkers:Int;
	final thread:Thread;

	public function new(maxWorkerThreads:Int) {
		this.maxWorkers = maxWorkerThreads;
		this.thread = Thread.current();
	}

	public function addJob<R>(job:()->R, callback:Callback<Exception,R>) {
		if(!active)
			throw new DeadJobExecutorException('Job executor has been shut down and does not accept new tasks');

		thread.events.run(() -> {
			if(ignoreOutcomes)
				return;
			try {
				var result = job();
				if(!ignoreOutcomes) {
					callback.success(result);
				}
			} catch(e) {
				if(!ignoreOutcomes) {
					callback.fail(e);
				}
			}
		});
	}

	public function isActive():Bool {
		return active;
	}

	public function shutdownNow() {
		if(!active)
			throw new DeadJobExecutorException('Cannot shutdown job executor as it has been shut down already');
		active = false;
		ignoreOutcomes = true;
	}

	public function shutdown() {
		if(!active)
			throw new DeadJobExecutorException('Cannot shutdown job executor as it has been shut down already');
		active = false;
	}
}