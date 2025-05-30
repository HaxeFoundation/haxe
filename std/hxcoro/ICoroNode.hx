package hxcoro;

import haxe.exceptions.CancellationException;
import haxe.coro.context.Context;
import haxe.coro.context.IElement;
import hxcoro.ICoroTask;

interface ICoroNode {
	var context(get, null):Context;
	function async<T>(lambda:NodeLambda<T>):ICoroTask<T>;
	function lazy<T>(lambda:NodeLambda<T>):IStartableCoroTask<T>;
	function cancel(?cause:CancellationException):Void;
	function with(...elements:IElement<Any>):ICoroNode;
}
