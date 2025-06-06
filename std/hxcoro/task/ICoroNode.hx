package hxcoro.task;

import haxe.exceptions.CancellationException;
import haxe.coro.context.Context;
import haxe.coro.context.IElement;
import hxcoro.task.ICoroTask;

interface ICoroNodeWith {
	var context(get, null):Context;
	function async<T>(lambda:NodeLambda<T>):ICoroTask<T>;
	function lazy<T>(lambda:NodeLambda<T>):IStartableCoroTask<T>;
	function with(...elements:IElement<Any>):ICoroNodeWith;
}

interface ICoroNode extends ICoroNodeWith extends ILocalContext {
	var id(get, never):Int;
	@:coroutine function awaitChildren():Void;
	function cancel(?cause:CancellationException):Void;
	function cancelChildren(?cause:CancellationException):Void;
}
