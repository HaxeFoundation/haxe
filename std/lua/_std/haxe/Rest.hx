package haxe;

import lua.Lua.select;
import lua.Table;
import lua.PairTools.copy;
import lua.TableTools.maxn;
import lua.TableTools.pack;
import lua.TableTools.unpack;
import haxe.iterators.RestIterator;
import haxe.iterators.RestKeyValueIterator;

private typedef NativeRest<T> = Table<Int,T>;

@:coreApi
abstract Rest<T>(NativeRest<T>) {
	public var length(get, never):Int;
	inline function get_length():Int
		return maxn(this);

	@:from static public function of<T>(array:Array<T>):Rest<T> {
		return new Rest(Table.fromArray(array));
	}

	inline function new(table:Table<Int,T>):Void
		this = table;

	@:arrayAccess inline function get(index:Int):T
		return this[index + 1];

	@:to public function toArray():Array<T> {
		return Table.toArray(this);
	}

	public inline function iterator():RestIterator<T>
		return new RestIterator<T>(this);

	public inline function keyValueIterator():RestKeyValueIterator<T>
		return new RestKeyValueIterator<T>(this);

	public inline function append(item:T):Rest<T> {
		var result = copy(this);
		Table.insert(result, item);
		return new Rest(result);
	}

	public inline function prepend(item:T):Rest<T> {
		var result = copy(this);
		Table.insert(result, 1, item);
		return new Rest(result);
	}

	public function toString():String {
		return toArray().toString();
	}
}