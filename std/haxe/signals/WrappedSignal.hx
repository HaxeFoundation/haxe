package haxe.signals;

import haxe.NoData;

/**
	An implementation of `haxe.async.Signal` which will listen for changes in its
	listeners. This is useful when a class changes its behavior depending on
	whether there are any listeners to some of its signals, e.g. a `Readable`
	stream will not emit data signals when there are no data handlers.
**/
class WrappedSignal<T> implements SignalEmitter<T> {
	final listeners:Array<Listener<T>> = [];
	public var changeSignal(get,never):Signal<NoData>;
	final _changeSignal = new ArraySignal<NoData>();

	function get_listenerCount():Int {
		return listeners.length;
	}

	public var listenerCount(get, never):Int;

	public function new() {}

	public function on(listener:Listener<T>):Void {
		listeners.push(listener);
		_changeSignal.emit(new NoData());
	}

	public function once(listener:Listener<T>):Void {
		listeners.push(function wrapped(data:T):Void {
			listeners.remove(wrapped);
			_changeSignal.emit(new NoData());
			listener(data);
		});
		_changeSignal.emit(new NoData());
	}

	public function off(?listener:Listener<T>):Void {
		if (listener != null) {
			listeners.remove(listener);
		} else {
			listeners.resize(0);
		}
		_changeSignal.emit(new NoData());
	}

	public function emit(data:T):Void {
		for (listener in listeners) {
			listener(data);
		}
	}

	inline function get_changeSignal():Signal<NoData> {
		return _changeSignal;
	}
}
