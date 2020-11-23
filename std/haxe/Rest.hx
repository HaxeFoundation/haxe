package haxe;

import haxe.iterators.RestIterator;
import haxe.iterators.RestKeyValueIterator;

private typedef NativeRest<T> = Array<T>;

/**
	A special type that represents "rest" function argument.

	Should be used as a type for the last argument of a method, representing that
	arbitrary number of arguments of given type can be passed to that method.

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
	**/
	@:from static public inline function of<T>(array:Array<T>):Rest<T>
		return new Rest(array);

	inline function new(array:Array<T>):Void
		this = array;

	@:arrayAccess inline function get(index:Int):T
		return this[index];

	@:to public inline function toArray():Array<T>
		return [for(i in 0...length) get(i)];

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
}