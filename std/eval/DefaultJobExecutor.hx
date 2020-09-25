package eval;

import haxe.Exception;
import haxe.Callback;
import haxe.NoData;
import haxe.IJobExecutor;
import sys.thread.Thread;
import sys.thread.Lock;
import sys.thread.Mutex;
import sys.thread.Deque;

/**
	Default implementation of `haxe.IJobExecutor` for eval (non-macro) target.
**/
class DefaultJobExecutor implements IJobExecutor {
	var workersMutex = new Mutex();
	var active = true;
	final workers = new Array<Worker>();
	final maxWorkers:Int;

	public function new(maxWorkerThreads:Int) {
		this.maxWorkers = maxWorkerThreads;
	}

	public function addJob<R>(job:()->R, callback:Callback<Exception,R>) {
		workersMutex.acquire();
		try {
			if(!active)
				throw new DeadJobExecutorException('Job executor has been shut down and does not accept new tasks');
			var scheduled = false;
			var leastLoaded = null;
			for(worker in workers) {
				if(worker.queueSize == 0) {
					worker.run(job, callback);
					scheduled = true;
					break;
				} else if(leastLoaded == null || leastLoaded.queueSize > worker.queueSize) {
					leastLoaded = worker;
				}
			}
			if(!scheduled) {
				if(workers.length < maxWorkers) {
					var worker = new Worker();
					workers.push(worker);
					worker.run(job, callback);
				} else {
					leastLoaded.run(job, callback);
				}
			}
		} catch(e) {
			workersMutex.release();
			throw e;
		}
		workersMutex.release();
	}

	public function isActive():Bool {
		return active;
	}

	public function shutdownNow() {
		shutdownWithHandler(w -> w.shutdownNow());
	}

	public function shutdown() {
		shutdownWithHandler(w -> w.shutdown());
	}

	inline function shutdownWithHandler(workerHandler:(w:Worker)->Void) {
		workersMutex.acquire();
		try {
			if(!active)
				throw new DeadJobExecutorException('Cannot shutdown job executor as it has been shut down already');
			active = false;
			for(worker in workers)
				workerHandler(worker);
		} catch(e) {
			workersMutex.release();
			throw e;
		}
		workersMutex.release();
	}
}

private class Worker {
	public var queueSize(default,null):Int = 0;
	public var ignoreOutcomes(default,null):Bool = false;

	var keepRunning = true;
	final thread:Thread;
	final queue = new Deque<Null<Task<Dynamic>>>();

	public function new() {
		thread = Thread.create(loop);
	}

	public function run<R>(job:()->R, callback:Callback<Exception,R>) {
		++queueSize;
		queue.add(new Task(job, callback, Thread.current(), this));
	}

	public function shutdown() {
		keepRunning = false;
		queue.push(null);
	}

	public function shutdownNow() {
		keepRunning = false;
		ignoreOutcomes = true;
		queue.push(null);
	}

	function loop() {
		while(keepRunning) {
			switch queue.pop(true) {
				case null:
				case task:
					if(!ignoreOutcomes) {
						task.run();
					}
					--queueSize;
			}
		}
	}
}

private class Task<R> {
	final job:()->R;
	final callback:Callback<Exception,R>;
	final thread:Thread;
	final worker:Worker;

	var result:Null<R>;
	var error:Null<Exception>;

	public function new(job:()->R, callback:Callback<Exception,R>, thread:Thread, worker:Worker) {
		this.job = job;
		this.callback = callback;
		this.worker = worker;
		this.thread = thread;
		thread.events.promise();
	}

	public function run() {
		try {
			result = job();
		} catch(e) {
			error = e;
		}
		thread.events.runPromised(invokeCallback);
	}

	function invokeCallback() {
		if(worker.ignoreOutcomes)
			return;

		switch error {
			case null: callback.success(result);
			case e: callback.fail(e);
		}
	}
}