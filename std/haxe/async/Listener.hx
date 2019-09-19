package haxe.async;

import haxe.NoData;

typedef ListenerData<T> = (data:T) -> Void;

/**
	Signal listener. A signal listener is a function which accepts one argument
	and has a `Void` return type.
**/
@:callable
abstract Listener<T>(ListenerData<T>) from ListenerData<T> {
	/**
		This function allows a listener to a `Signal<NoData>` to be defined as a
		function which accepts no arguments.
	**/
	@:from static inline function fromNoArguments(f:() -> Void):Listener<NoData>
		return(data:NoData) -> f();
}
