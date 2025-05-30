package hxcoro;

import haxe.Exception;
import haxe.exceptions.CancellationException;

interface ICoroTask<T> {
	function cancel(?cause:CancellationException):Void;
	@:coroutine function await():T;
	function get():T;
	function getError():Exception;
	function isActive():Bool;
}

interface IStartableCoroTask<T> extends ICoroTask<T> {
	function start():Void;
}
