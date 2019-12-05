package haxe.signals;

/**
	Signals are a type-safe system to listen for events. A signal will call its
	listeners whenever _something_ (the event that the signal represents) happens,
	passing along any relevant associated data.

	Signals which have no associated data should use `haxe.NoData` as their type
	parameter.

	Signals should be used in conjunction with something like `haxe.Emitter`, which would
	actually emit a signal.
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
	function off(?listener:Listener<T>):Void;
}
