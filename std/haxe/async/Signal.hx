package haxe.async;

/**
	Signals are a type-safe system to emit events. A signal will calls its
	listeners whenever _something_ (the event that the signal represents) happens,
	passing along any relevant associated data.

	Signals which have no associated data should use `haxe.NoData` as their type
	parameter.
**/
interface Signal<T> {
	/**
		Number of listeners to `this` signal.
	**/
	var listenerCount(get, never):Int;

	/**
		Adds a listener to `this` signal, which will be called for all signal
		emissions until it is removed with `off`.
	**/
	function on(listener:Listener<T>):Void;

	/**
		Adds a listener to `this` signal, which will be called only once, the next
		time the signal emits.
	**/
	function once(listener:Listener<T>):Void;

	/**
		Removes the given listener from `this` signal.
	**/
	function off(listener:Listener<T>):Void;

	/**
		Removes all listeners from `this` signal.
	**/
	function clear():Void;

	/**
		Emits `data` to all current listeners of `this` signal.
	**/
	function emit(data:T):Void;
}
