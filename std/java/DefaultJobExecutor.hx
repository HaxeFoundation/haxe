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
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.Executors;

@:native("haxe.java.DefaultJobExecutor")
class DefaultJobExecutor implements IJobExecutor {
	final service:HxThreadPoolExecutor;
	var active = true;

	public function new(maxThreadsCount:Int) {
		service = new HxThreadPoolExecutor(
			maxThreadsCount, maxThreadsCount, 60, SECONDS,
			new LinkedBlockingQueue<Runnable>(), new DaemonThreadFactory()
		);
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

@:native("haxe.java.DaemonThreadFactory")
private class DaemonThreadFactory implements ThreadFactory {
	final factory:ThreadFactory;

	public function new() {
		factory = Executors.defaultThreadFactory();
	}

	public function newThread(r) {
		var javaThread = factory.newThread(r);
		javaThread.setDaemon(true);
		return javaThread;
	}
}

@:native("haxe.java._DefaultJobExecutor.Task")
private class Task<R> implements Runnable {
	public final job:()->R;
	public final thread:Thread;
	public final callback:Callback<Exception,R>;

	var result:Null<R>;
	var error:Null<Exception>;

	public function new(job:()->R, thread:Thread, callback:Callback<Exception,R>) {
		this.job = job;
		this.thread = thread;
		this.callback = callback;
		thread.events.promise();
	}

	public function run() {
		try {
			result = job();
		} catch(e) {
			error = e;
		}
	}

	public function submitOutcome() {
		thread.events.runPromised(() -> {
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