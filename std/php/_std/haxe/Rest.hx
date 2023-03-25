package haxe;

import php.*;
import haxe.iterators.RestIterator;
import haxe.iterators.RestKeyValueIterator;

private typedef NativeRest<T> = NativeIndexedArray<T>;

@:coreApi
@:semantics(value)
abstract Rest<T>(NativeRest<T>) {
	public var length(get,never):Int;
	inline function get_length():Int
		return Global.count(this);

	@:from
	static public inline function of<T>(array:Array<T>):Rest<T>
		return new Rest(@:privateAccess array.arr);

	@:noDoc
	@:from
	static inline function ofNative<T>(array:NativeIndexedArray<T>):Rest<T>
		return new Rest(array);

	inline function new(a:NativeIndexedArray<T>):Void
		this = a;

	@:arrayAccess inline function get(index:Int):T
		return this[index];

	@:to public inline function toArray():Array<T>
		return @:privateAccess Array.wrap(this);

	public inline function iterator():RestIterator<T>
		return new RestIterator<T>(this);

	public inline function keyValueIterator():RestKeyValueIterator<T>
		return new RestKeyValueIterator<T>(this);

	public inline function append(item:T):Rest<T> {
		var result = this;
		result.push(item);
		return new Rest(result);
	}

	public inline function prepend(item:T):Rest<T> {
		var result = this;
		Global.array_unshift(result, item);
		return new Rest(result);
	}

	public function toString():String {
		return inline Boot.stringifyNativeIndexedArray(this);
	}
}