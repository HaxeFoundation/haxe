package php;

import haxe.Exception;
import haxe.Callback;
import haxe.NoData;
import haxe.EntryPoint;
import haxe.IJobExecutor;

/**
	Default implementation of `haxe.IJobExecutor` for php target.
**/
class DefaultJobExecutor implements IJobExecutor {
	var jobs = new NativeIndexedArray<()->Void>();
	var active = true;
	var ignoreOutcomes = false;
	var addedToEntryPoint = false;

	public function new() {}

	public function addJob<R>(job:()->R, callback:Callback<Exception,R>) {
		if(!active)
			throw new DeadJobExecutorException('Job executor has been shut down and does not accept new tasks');
		pushJob(job, callback);
	}

	inline function pushJob<R>(job:()->R, callback:Callback<Exception,R>) {
		jobs.push(() -> {
			if(ignoreOutcomes)
				return;
			var result = try {
				job();
			} catch(e) {
				if(!ignoreOutcomes)
					callback.fail(e);
				return;
			}
			if(!ignoreOutcomes)
				callback.success(result);
		});
		schedule();
	}

	public function isActive():Bool {
		return active;
	}

	public function shutdownNow() {
		if(!active)
			throw new DeadJobExecutorException('Cannot shutdown job executor as it has been shut down already');
		active = false;
		ignoreOutcomes = true;
		jobs = new NativeIndexedArray();
	}

	public function shutdown() {
		if(!active)
			throw new DeadJobExecutorException('Cannot shutdown job executor as it has been shut down already');
		active = false;
		ignoreOutcomes = false;
	}

	function process() {
		while(!ignoreOutcomes) {
			switch Global.array_shift(jobs) {
				case null: return;
				case job: job();
			}
		}
	}

	inline function schedule() {
		if(!addedToEntryPoint)
			EntryPoint.runInMainThread(() -> {
				addedToEntryPoint = false;
				process();
			});
	}
}