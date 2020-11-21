package haxe;

import php.*;

@:coreApi
@:coreType
@:using(haxe.Rest)
@:semantics(value)
abstract Rest<T> {
	public var length(get,never):Int;
	inline function get_length():Int
		return Global.count(this);

	@:from
	static public inline function of<T>(array:Array<T>):Rest<T>
		return cast @:privateAccess array.arr;

	@:arrayAccess function get(index:Int):T;

	@:to public inline function toArray():Array<T> {
		return [for(i in 0...length) this[i]];
	}

	public inline function iterator():RestIterator<T> {
		return new RestIterator<T>(cast this);
	}

	public inline function keyValueIterator():RestKeyValueIterator<T> {
		return new RestKeyValueIterator<T>(cast this);
	}
}

private class RestIterator<T> {
	final args:NativeIndexedArray<T>;
	final length:Int;
	var current:Int = 0;

	public inline function new(args:NativeIndexedArray<T>) {
		length = Global.count(args);
		this.args = args;
	}

	public inline function hasNext():Bool {
		return current < length;
	}

	public inline function next():T {
		return args[current++];
	}
}

private class RestKeyValueIterator<T> {
	final args:NativeIndexedArray<T>;
	final length:Int;
	var current:Int = 0;

	public inline function new(args:NativeIndexedArray<T>) {
		length = Global.count(args);
		this.args = args;
	}

	public inline function hasNext():Bool {
		return current < length;
	}

	public inline function next():{key:Int, value:T} {
		return {key:current, value:args[current++]};
	}
}