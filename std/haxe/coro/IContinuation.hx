package haxe.coro;

import haxe.Exception;

interface IContinuation<T> {
	final context:CoroutineContext;

	function resume(result:T, error:Exception):Void;
}
