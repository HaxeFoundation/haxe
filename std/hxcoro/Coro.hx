package hxcoro;

import haxe.coro.IContinuation;
import haxe.coro.SuspensionResult;
import haxe.coro.schedulers.Scheduler;
import haxe.exceptions.CancellationException;

class Coro {
	@:coroutine @:coroutine.transformed
	public static function suspend<T>(func:haxe.coro.IContinuation<T>->Void, completion:haxe.coro.IContinuation<T>):T {
		var safe = new haxe.coro.continuations.RacingContinuation(completion);
		func(safe);
		safe.resolve();
		return cast safe;
	}

	static function cancellationRequested(cont:IContinuation<Any>) {
		return cont.context.get(hxcoro.CoroTask.key)?.cancellationRequested();
	}

	@:coroutine @:coroutine.nothrow public static function delay(ms:Int):Void {
		suspend(cont -> {
			cont.context.get(Scheduler.key).schedule(ms, () -> {
				cont.resume(null, cancellationRequested(cont) ? new CancellationException() : null);
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
			final scope = new CoroScopeTask(context, lambda);
			scope.awaitContinuation(cont);
		});
	}
}