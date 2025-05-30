package haxe.coro;

import haxe.Exception;
import haxe.coro.context.Context;

interface IContinuation<T> {
	var context(get, never):Context;

	function resume(result:T, error:Exception):Void;
}
