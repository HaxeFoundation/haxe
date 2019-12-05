package haxe.signals;

/**
	Emitters allow to emit some data to whatever might be waiting for that data.

	E.g. can be used in conjunction with `haxe.signals.Signal` to implement a class,
	which can emit an event and notify listeners of that event.
**/
interface Emitter<T> {
	function emit(data:T):Void;
}
