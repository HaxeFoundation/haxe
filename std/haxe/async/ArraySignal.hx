package haxe.async;

/**
	Basic implementation of a `haxe.async.Signal`. Uses an array for storing
	listeners for the signal.
**/
class ArraySignal<T> implements Signal<T> {
	final listeners:Array<Listener<T>> = [];

	function get_listenerCount():Int {
		return listeners.length;
	}

	public var listenerCount(get, never):Int;

	public function new() {}

	public function on(listener:Listener<T>):Void {
		listeners.push(listener);
	}

	public function once(listener:Listener<T>):Void {
		listeners.push(function wrapped(data:T):Void {
			listeners.remove(wrapped);
			listener(data);
		});
	}

	public function off(listener:Listener<T>):Void {
		listeners.remove(listener);
	}

	public function clear():Void {
		listeners.resize(0);
	}

	public function emit(data:T):Void {
		for (listener in listeners) {
			listener(data);
		}
	}
}
