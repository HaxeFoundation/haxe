package haxe.iterators;

import haxe.ds.HashMap;

class HashMapKeyValueIterator<K:{function hashCode():Int;}, V> {
	final map:HashMap<K, V>;
	final keys:Iterator<K>;

	public inline function new(map:HashMap<K, V>) {
		this.map = map;
		this.keys = map.keys();
	}

	/**
		See `Iterator.hasNext`
	**/
	public inline function hasNext():Bool {
		return keys.hasNext();
	}

	/**
		See `Iterator.next`
	**/
	public inline function next():{key:K, value:V} {
		var key = keys.next();
		return {value: map.get(key), key: key};
	}
}
