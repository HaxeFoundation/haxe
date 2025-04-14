package haxe.coro;

import haxe.Exception;

interface IContinuation<T> {
	final _hx_context:CoroutineContext;
	var _hx_recursing:Bool;

	function resume(result:T, error:Exception):Void;
}
