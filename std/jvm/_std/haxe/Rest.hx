package haxe;

import haxe.ds.Vector;
import haxe.iterators.RestIterator;
import haxe.iterators.RestKeyValueIterator;

private typedef NativeRest<T> = Vector<T>;

@:coreApi
abstract Rest<T>(NativeRest<T>) {
	public var length(get, never):Int;

	inline function get_length():Int
		return this.length;

	inline function new(a:NativeRest<T>) {
		this = a;
	}

	@:arrayAccess inline function get(index:Int):T
		return this[index];

	@:from extern inline static public function of<T>(array:Array<T>):Rest<T> {
		var result = new NativeRest(array.length);
		for (i in 0...array.length)
			result[i] = array[i];
		return new Rest(result);
	}

	@:noDoc
	@:from extern inline static function fromNative<T>(a:java.NativeArray<T>):Rest<T> {
		return new Rest(Vector.fromData(a));
	}

	extern inline public function append(item:T):Rest<T> {
		var r = new NativeRest(length + 1);
		Vector.blit(this, 0, r, 0, length);
		r[length] = item;
		return new Rest(r);
	}

	extern inline public function prepend(item:T):Rest<T> {
		var r = new NativeRest(length + 1);
		Vector.blit(this, 0, r, 1, length);
		r[0] = item;
		return new Rest(r);
	}

	public inline function iterator():RestIterator<T>
		return new RestIterator<T>(this);

	public inline function keyValueIterator():RestKeyValueIterator<T>
		return new RestKeyValueIterator<T>(this);

	@:to public inline function toArray():Array<T> {
		return [for (i in 0...this.length) this[i]];
	}

	public inline function toString():String {
		return toArray().toString();
	}
}
