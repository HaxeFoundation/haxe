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
	var shutdownCallback:Null<Callback<Exception,NoData>>;
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
					jobFail(callback, e);
				return;
			}
			if(!ignoreOutcomes)
				jobSuccess(callback, result);
		});
		schedule();
	}

	public function addAsyncJob<E,R>(job:(callback:Callback<E,R>)->Void, callback:Callback<Exception,R>) {
		if(!active)
			throw new DeadJobExecutorException('Job executor has been shut down and does not accept new tasks');
		jobs.push(() -> {
			if(ignoreOutcomes)
				return;
			var synchronous = true;
			try {
				job((e, r) -> {
					if(!ignoreOutcomes) {
						// This is required to move `callback` execution out of the
						// `try...catch` in case `job` is completed synchronously.
						if(synchronous)
							pushJob(e == null ? () -> r : () -> throw e, callback)
						else if(e == null)
							jobSuccess(callback, r)
						else
							jobFail(callback, @:privateAccess Exception.caught(e));
					}
				});
				synchronous = false;
			} catch(e) {
				synchronous = false;
				if(!ignoreOutcomes) {
					jobFail(callback, e);
					return;
				}
			}
		});
		schedule();
	}

	public function isActive():Bool {
		return active;
	}

	public function shutdownNow() {
		active = false;
		ignoreOutcomes = true;
		jobs = new NativeIndexedArray();
	}

	public function shutdown(callback:Callback<Exception,NoData>) {
		if(!active)
			throw new DeadJobExecutorException('Cannot gracefully shutdown job executor as it has been shut down already');
		active = false;
		ignoreOutcomes = false;
		shutdownCallback = callback;
	}

	inline function jobSuccess<E,R>(callback:Callback<E,R>, result:R) {
		callback.success(result);
		if(!active && shutdownCallback != null && Global.count(jobs) == 0)
			shutdownCallback.success(NoData);
	}

	inline function jobFail<E,R>(callback:Callback<E,R>, e:E) {
		callback.fail(e);
		if(!active && shutdownCallback != null && Global.count(jobs) == 0)
			shutdownCallback.success(NoData);
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