package haxe;

import js.Syntax;
import haxe.iterators.RestIterator;
import haxe.iterators.RestKeyValueIterator;

@:coreApi
@:coreType
@:using(haxe.Rest)
abstract Rest<T> {
	public var length(get,never):Int;
	inline function get_length():Int {
		return Syntax.code('{0}.length', this);
	}

	@:from static public inline function of<T>(array:Array<T>):Rest<T>
		return cast array;

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
		var result = (cast this:Array<T>).copy();
		result.push(item);
		return result;
	}

	public inline function prepend(item:T):Rest<T> {
		var result = (cast this:Array<T>).copy();
		result.unshift(item);
		return result;
	}
}