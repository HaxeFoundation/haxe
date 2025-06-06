package hxcoro.task;

import haxe.Exception;
import haxe.exceptions.CancellationException;

interface ICoroTask<T> extends ILocalContext {
	var id(get, never):Int;
	function cancel(?cause:CancellationException):Void;
	@:coroutine function await():T;
	function get():T;
	function getError():Exception;
	function isActive():Bool;
}

interface IStartableCoroTask<T> extends ICoroTask<T> {
	function start():Void;
}
