package hxcoro.task;

import haxe.coro.context.Key;

interface ILocalContext {
	function getLocalElement<T>(key:Key<T>):Null<T>;
	function setLocalElement<T>(key:Key<T>, element:T):Void;
}