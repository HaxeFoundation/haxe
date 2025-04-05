package haxe;

import haxe.iterators.RestIterator;
import haxe.iterators.RestKeyValueIterator;

private typedef NativeRest<T> = Array<T>;

/**
	A special type that represents a "rest" function argument.

	The special `...` syntax can be used for convenience and improved readability:

	```haxe
	function f(...rest:Int) {
		$type(rest); // haxe.Rest<Int>
	}

	f(1, 2, 3);

	final array = [1, 2, 3];
	f(...array);
	```

	Should be used as a type for the last argument of a method, indicating that
	an arbitrary number of arguments of the given type can be passed to that method.

	Allows to use array access by index to get values of rest arguments.
	If the index exceeds the amount of rest arguments passed, the result is unspecified.
**/
@:coreApi
abstract Rest<T>(NativeRest<T>) {
	/** Amount of arguments passed as rest arguments */
	public var length(get,never):Int;
	inline function get_length():Int
		return this.length;

	/**
		Create rest arguments using contents of `array`.

		WARNING:
		Depending on a target platform modifying `array` after using this method
		may affect the created `Rest` instance.
		Use `Rest.of(array.copy())` to avoid that.
	**/
	@:from static public inline function of<T>(array:Array<T>):Rest<T>
		return new Rest(array);

	inline function new(array:Array<T>):Void
		this = array;

	@:arrayAccess inline function get(index:Int):T
		return this[index];

	/**
		Creates an array containing all the values of rest arguments.
	**/
	@:to public #if !cppia inline #end function toArray():Array<T>
		return this.copy();

	public inline function iterator():RestIterator<T>
		return new RestIterator<T>(this);

	public inline function keyValueIterator():RestKeyValueIterator<T>
		return new RestKeyValueIterator<T>(this);

	/**
		Create a new rest arguments collection by appending `item` to this one.
	**/
	public function append(item:T):Rest<T> {
		var result = this.copy();
		result.push(item);
		return new Rest(result);
	}

	/**
		Create a new rest arguments collection by prepending this one with `item`.
	**/
	public function prepend(item:T):Rest<T> {
		var result = this.copy();
		result.unshift(item);
		return new Rest(result);
	}

	public inline function toString():String {
		#if (flash || js)
			return '[${this.toString()}]';
		#else
			return this.toString();
		#end
	}
}