package haxe.coro.dispatchers;

/**
	Classes that implement this interface can be passed to instances of
	`haxe.coro.dispatchers.Dispatcher`.
**/
interface IDispatchObject {
	/**
		This function is invoked when a scheduled object is run.
	**/
	function onDispatch():Void;
}