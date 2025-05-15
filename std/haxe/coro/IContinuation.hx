package haxe.coro;

import haxe.Exception;
import haxe.coro.context.Context;

interface IContinuation<T> {
	final context:Context;

	function resume(result:T, error:Exception):Void;
}
