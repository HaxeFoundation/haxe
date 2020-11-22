package haxe;

import php.*;
import haxe.iterators.RestIterator;
import haxe.iterators.RestKeyValueIterator;

@:coreApi
@:coreType
@:using(haxe.Rest)
@:semantics(value)
abstract Rest<T> to NativeIndexedArray<T> to NativeArray {
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
		return new RestIterator(this);
	}

	public inline function keyValueIterator():RestKeyValueIterator<T> {
		return new RestKeyValueIterator(this);
	}

	public inline function append(item:T):Rest<T> {
		var result:NativeIndexedArray<T> = this;
		result.push(item);
		return cast result;
	}

	public inline function prepend(item:T):Rest<T> {
		var result:NativeIndexedArray<T> = this;
		Global.array_unshift(result, item);
		return cast result;
	}
}