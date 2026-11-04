package unit.issues;

import haxe.Constraints.IMap;
import haxe.ds.StringMap;

class Issue7592 extends Test {
	function test() {
		var xmap = new XMap();
		xmap["1"] = "one";
		xmap.set("2", "two");
		eq("one", xmap["1"]);
		eq("two", xmap.get("2"));
		t(xmap.exists("1"));
		t(xmap.remove("1"));
		f(xmap.exists("1"));

		#if !cppia // https://github.com/HaxeFoundation/hxcpp/pull/1460
		var imap:IMap<String, String> = new XStringMap();
		imap.set("3", "three");
		eq("three", imap.get("3"));
		eq(1, imap.size());
		#end
	}
}

private interface IXMap<K, V> extends IMap<K, V> {}

@:multiType(@:followWithAbstracts K)
private abstract XMap<K, V>(IXMap<K, V>) {
	public function new();

	public inline function set(key:K, value:V):Void
		this.set(key, value);

	@:arrayAccess public inline function get(key:K):Null<V>
		return this.get(key);

	public inline function exists(key:K):Bool
		return this.exists(key);

	public inline function remove(key:K):Bool
		return this.remove(key);

	@:arrayAccess inline function arrayWrite(k:K, v:V):V {
		this.set(k, v);
		return v;
	}

	@:to static inline function toXStringMap<K:String, V>(t:IXMap<K, V>):XStringMap<V> {
		return new XStringMap<V>();
	}
}

private class XStringMap<T> implements IXMap<String, T> {
	var map:StringMap<T> = new StringMap();

	public function new():Void {}

	public function set(key:String, value:T):Void {
		map.set(key, value);
	}

	public inline function get(key:String):Null<T> {
		return map.get(key);
	}

	public inline function exists(key:String):Bool {
		return map.exists(key);
	}

	public function remove(key:String):Bool {
		return map.remove(key);
	}

	public inline function keys():Iterator<String> {
		return map.keys();
	}

	public inline function iterator():Iterator<T> {
		return map.iterator();
	}

	public inline function keyValueIterator():KeyValueIterator<String, T> {
		return new haxe.iterators.MapKeyValueIterator(this);
	}

	public function copy():XStringMap<T> {
		var clone = new XStringMap();
		clone.map = map.copy();
		return clone;
	}

	public function clear():Void
		map.clear();

	public function size():Int
		return map.size();

	public function toString():String {
		return map.toString();
	}
}
