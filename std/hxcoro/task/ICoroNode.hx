package hxcoro.task;

import haxe.exceptions.CancellationException;
import haxe.coro.context.Context;
import haxe.coro.context.IElement;
import hxcoro.task.ICoroTask;

interface ICoroNodeWith<C = Any> {
	var context(get, null):Context;
	function async<T:C, R>(lambda:NodeLambda<T, R>):ICoroTask<T>;
	function lazy<T:C, R>(lambda:NodeLambda<T, R>):IStartableCoroTask<T>;
	function with(...elements:IElement<Any>):ICoroNodeWith<C>;
}

interface ICoroNode<C = Any> extends ICoroNodeWith<C> {
	var id(get, never):Int;
	@:coroutine function awaitChildren():Void;
	function cancel(?cause:CancellationException):Void;
	function cancelChildren(?cause:CancellationException):Void;
}
