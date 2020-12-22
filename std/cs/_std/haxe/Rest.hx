package haxe;

import haxe.iterators.RestIterator;
import haxe.iterators.RestKeyValueIterator;
import cs.NativeArray;
import cs.system.Array as CsArray;

private typedef NativeRest<T> = #if erase_generics NativeArray<Dynamic> #else NativeArray<T> #end;

@:coreApi
abstract Rest<T>(NativeRest<T>) {
	public var length(get,never):Int;
	inline function get_length():Int
		return this.Length;

	@:from static public inline function of<T>(array:Array<T>):Rest<T>
		return new Rest(@:privateAccess array.__a);

	inline function new(a:NativeRest<T>):Void
		this = a;

	@:arrayAccess inline function get(index:Int):T
		return this[index];

	@:to public function toArray():Array<T> {
		var result = new NativeRest(this.Length);
		CsArray.Copy(this, 0, result, 0, this.Length);
		return @:privateAccess Array.ofNative(result);
	}

	public inline function iterator():RestIterator<T>
		return new RestIterator<T>(this);

	public inline function keyValueIterator():RestKeyValueIterator<T>
		return new RestKeyValueIterator<T>(this);

	public function append(item:T):Rest<T> {
		var result = new NativeRest(this.Length + 1);
		CsArray.Copy(this, 0, result, 0, this.Length);
		result[this.Length] = item;
		return new Rest(result);
	}

	public function prepend(item:T):Rest<T> {
		var result = new NativeRest(this.Length + 1);
		CsArray.Copy(this, 0, result, 1, this.Length);
		result[0] = item;
		return new Rest(result);
	}

	public function toString():String {
		return toArray().toString();
	}
}