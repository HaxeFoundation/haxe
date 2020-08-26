package java;

import haxe.IJobExecutor;
import haxe.Callback;
import haxe.Exception;
import sys.thread.Thread;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.lang.Runnable;
import java.lang.Throwable;

@:native("haxe.java.DefaultJobExecutor")
class DefaultJobExecutor implements IJobExecutor {
	final service:HxThreadPoolExecutor;
	var active = true;

	public function new(maxThreadsCount:Int) {
		service = new HxThreadPoolExecutor(maxThreadsCount, maxThreadsCount, 60, SECONDS, new LinkedBlockingQueue<Runnable>());
		service.allowCoreThreadTimeOut(true);
	}

	public function addJob<R>(job:()->R, callback:Callback<Exception,R>):Void {
		if(!active)
			throw new DeadJobExecutorException('Job executor has been shut down and does not accept new tasks');

		service.execute(new Task(job, Thread.current(), callback));
	}

	public function isActive():Bool {
		return !service.isShutdown();
	}

	public function shutdownNow():Void {
		if(service.isShutdown())
			throw new DeadJobExecutorException('Cannot shutdown job executor as it has been shut down already');
		service.shutdownNow();
	}

	public function shutdown():Void {
		if(service.isShutdown())
			throw new DeadJobExecutorException('Cannot shutdown job executor as it has been shut down already');
		service.shutdown();
	}
}

@:native("haxe.java._DefaultJobExecutor.Task")
private class Task<R> implements Runnable {
	public final job:()->R;
	public final caller:Thread;
	public final callback:Callback<Exception,R>;

	var result:Null<R>;
	var error:Null<Exception>;

	public function new(job:()->R, caller:Thread, callback:Callback<Exception,R>) {
		this.job = job;
		this.caller = caller;
		this.callback = callback;
		caller.promiseEvent();
	}

	public function run() {
		try {
			result = job();
		} catch(e) {
			error = e;
		}
	}

	public function submitOutcome() {
		caller.schedulePromisedEvent(() -> {
			if(error == null)
				callback.success(result)
			else
				callback.fail(error);
		});
	}
}

@:native("haxe.java._DefaultJobExecutor.HxThreadPoolExecutor")
private class HxThreadPoolExecutor extends ThreadPoolExecutor {
	override overload function afterExecute(r:Runnable, t:Throwable) {
		super.afterExecute(r, t);
		(cast r:Task<Dynamic>).submitOutcome();
	}
}