package hxcoro;

import haxe.coro.IContinuation;
import haxe.coro.schedulers.Scheduler;
import haxe.coro.schedulers.ISchedulerHandle;
import haxe.coro.cancellation.CancellationToken;
import haxe.coro.cancellation.ICancellationHandle;
import haxe.exceptions.CancellationException;
import haxe.exceptions.ArgumentException;
import hxcoro.task.NodeLambda;
import hxcoro.task.CoroScopeTask;
import hxcoro.exceptions.TimeoutException;
import hxcoro.continuations.TimeoutContinuation;

class Coro {
	@:coroutine @:coroutine.transformed
	public static function suspend<T>(func:haxe.coro.IContinuation<T>->Void, completion:haxe.coro.IContinuation<T>):T {
		var safe = new haxe.coro.continuations.RacingContinuation(completion);
		func(safe);
		safe.resolve();
		return cast safe;
	}

	static function cancellationRequested(cont:IContinuation<Any>) {
		return cont.context.get(CancellationToken.key)?.isCancellationRequested;
	}

	@:coroutine @:coroutine.nothrow public static function delay(ms:Int):Void {
		suspend(cont -> {
			var scheduleHandle:ISchedulerHandle = null;
			var cancellationHandle:ICancellationHandle = null;

			final ct = cont.context.get(CancellationToken.key);

			scheduleHandle = cont.context.get(Scheduler.key).schedule(ms, () -> {
				cancellationHandle.close();
				cont.resume(null, ct.isCancellationRequested ? new CancellationException() : null);
			});

			cancellationHandle = ct.onCancellationRequested(() -> {
				scheduleHandle.close();
				cont.resume(null, new CancellationException());
			});
		});
	}

	@:coroutine @:coroutine.nothrow public static function yield():Void {
		suspend(cont -> {
			cont.context.get(Scheduler.key).schedule(0, () -> {
				cont.resume(null, cancellationRequested(cont) ? new CancellationException() : null);
			});
		});
	}

	@:coroutine static public function scope<T>(lambda:NodeLambda<T>):T {
		return suspend(cont -> {
			final context = cont.context;
			final scope = new CoroScopeTask(context);
			scope.runNodeLambda(lambda);
			scope.awaitContinuation(cont);
		});
	}

	/**
	 * Runs the provided lambda with a timeout, if the timeout is exceeded this functions throws `hxcoro.exceptions.TimeoutException`.
	 * If a timeout of zero is provided the function immediately throws `hxcoro.exceptions.TimeoutException`.
	 * @param ms Timeout in milliseconds.
	 * @param lambda Lambda function to execute.
	 * @throws `hxcoro.exceptions.TimeoutException` If the timeout is exceeded.
	 * @throws `haxe.ArgumentException` If the `ms` parameter is less than zero.
	 */
	@:coroutine public static function timeout<T>(ms:Int, lambda:NodeLambda<T>):T {
		return suspend(cont -> {
			if (ms < 0) {
				cont.resume(null, new ArgumentException('timeout must be positive'));

				return;
			}
			if (ms == 0) {
				cont.resume(null, new TimeoutException());

				return;
			}

			final context = cont.context;
			final scope = new CoroScopeTask(context);
			final handle = context.get(Scheduler.key).schedule(ms, () -> {
				scope.cancel(new TimeoutException());
			});

			scope.runNodeLambda(lambda);
			scope.awaitContinuation(new TimeoutContinuation(cont, handle));
		});
	}
}
