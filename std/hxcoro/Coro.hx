package hxcoro;

import hxcoro.continuations.CancellingContinuation;
import haxe.coro.IContinuation;
import haxe.coro.ICancellableContinuation;
import haxe.coro.schedulers.Scheduler;
import haxe.coro.cancellation.CancellationToken;
import haxe.exceptions.CancellationException;
import haxe.exceptions.ArgumentException;
import hxcoro.task.NodeLambda;
import hxcoro.task.CoroTask;
import hxcoro.exceptions.TimeoutException;
import hxcoro.continuations.TimeoutContinuation;

class Coro {
	@:coroutine @:coroutine.transformed
	public static function suspend<T>(func:IContinuation<T>->Void, completion:IContinuation<T>):T {
		var safe = new haxe.coro.continuations.RacingContinuation(completion);
		func(safe);
		safe.resolve();
		return cast safe;
	}

	/**
	 * Suspends a coroutine which will be automatically resumed with a `haxe.exceptions.CancellationException` when cancelled.
	 * The `ICancellableContinuation` passed to the function allows registering a callback which is invoked on cancellation
	 * allowing the easy cleanup of resources.
	 */
	@:coroutine @:coroutine.transformed public static function suspendCancellable<T>(func:ICancellableContinuation<T>->Void, completion:IContinuation<T>):T {
		var safe = new CancellingContinuation(completion);
		func(safe);
		return cast safe;
	}

	static function cancellationRequested(cont:IContinuation<Any>) {
		return cont.context.get(CancellationToken)?.isCancellationRequested();
	}

	static function delayImpl<T>(ms:Int, cont:ICancellableContinuation<T>) {
		final handle = cont.context.get(Scheduler).schedule(ms, () -> {
			cont.callSync();
		});

		cont.onCancellationRequested = _ -> {
			handle.close();
		}
	}

	@:coroutine @:coroutine.nothrow public static function delay(ms:Int):Void {
		suspendCancellable(cont -> delayImpl(ms, cont));
	}

	@:coroutine @:coroutine.nothrow public static function yield():Void {
		suspendCancellable(cont -> delayImpl(0, cont));
	}

	@:coroutine static public function scope<T>(lambda:NodeLambda<T>):T {
		return suspend(cont -> {
			final context = cont.context;
			final scope = new CoroTask(context, CoroTask.CoroScopeStrategy);
			scope.runNodeLambda(lambda);
			scope.awaitContinuation(cont);
		});
	}

	/**
		Executes `lambda` in a new task, ignoring all child exceptions.

		The task itself can still raise an exception. This is also true when calling
		`child.await()` on a child that raises an exception.
	**/
	@:coroutine static public function supervisor<T>(lambda:NodeLambda<T>):T {
		return suspend(cont -> {
			final context = cont.context;
			final scope = new CoroTask(context, CoroTask.CoroSupervisorStrategy);
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
				cont.failSync(new ArgumentException('timeout must be positive'));

				return;
			}
			if (ms == 0) {
				cont.failSync(new TimeoutException());

				return;
			}

			final context = cont.context;
			final scope = new CoroTask(context, CoroTask.CoroScopeStrategy);
			final handle = context.get(Scheduler).schedule(ms, () -> {
				scope.cancel(new TimeoutException());
			});

			scope.runNodeLambda(lambda);
			scope.awaitContinuation(new TimeoutContinuation(cont, handle));
		});
	}
}
