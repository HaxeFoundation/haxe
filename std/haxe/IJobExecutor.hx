package haxe;

class DeadJobExecutorException extends Exception {}

/**
	An interface to execute jobs.

	Depending on an implementation a call to `shutdown` or `shutdownNow` may be
	required to free up allocated resources.
**/
interface IJobExecutor {
	/**
		Schedule a new `job` to execute.

		Return value of the `job` is passed as a result to the `callback`.

		If the `job` throws an exception it will be passed as an error to the
		`callback`.

		@throws haxe.IJobExecutor.DeadJobExecutor if this executor has been shut down.
	**/
	function addJob<R>(job:()->R, callback:Callback<Exception,R>):Void;

	/**
		Returns `true` if this executor is active and accepts new jobs.
		Returns `false` if this executor has been shut down.
	**/
	function isActive():Bool;

	/**
		Shutdown immediately.
		Tries to cancel any ongoing jobs.
		Ignores outcome of any job, which may finish after the shutdown.

		Any new jobs will be rejected with an exception.

		@throws haxe.IJobExecutor.DeadJobExecutor if this executor has been shut down already.
	**/
	function shutdownNow():Void;

	/**
		Shutdown gracefully.
		Waits for existing jobs to complete.

		Any new jobs will be rejected with an exception.

		@throws haxe.IJobExecutor.DeadJobExecutor if this executor has been shut down already.
	**/
	function shutdown():Void;
}