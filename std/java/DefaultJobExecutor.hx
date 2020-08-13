package java;

import haxe.IJobExecutor;
import haxe.Callback;
import haxe.Exception;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.lang.Runnable;
import java.lang.Throwable;

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

		service.execute(new Task(job, callback));
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

private class Task<R> implements Runnable {
	public final job:()->R;
	public final callback:Callback<Exception,R>;

	var result:Null<R>;
	var error:Null<Exception>;

	public function new(job:()->R, callback:Callback<Exception,R>) {
		this.job = job;
		this.callback = callback;
	}

	public function run() {
		try {
			result = job();
		} catch(e) {
			error = e;
		}
	}

	public function submitOutcome() {
		if(error == null)
			callback.success(result)
		else
			callback.fail(error);
	}
}

private class HxThreadPoolExecutor extends ThreadPoolExecutor {
	override overload function afterExecute(r:Runnable, t:Throwable) {
		super.afterExecute(r, t);
		(cast r:Task<Dynamic>).submitOutcome();
	}
}