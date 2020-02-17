package unit.issues;

class Issue6833 extends unit.Test {
	function test() {
		for(p in new KeyValueIterator([1 => 'hello'])) {}
		noAssert();
	}
}

private class KeyValueIterator<K,V> {
	var map:Map<K,V>;
	var keys:Iterator<K>;

	public inline function new(map:Map<K,V>) {
		this.map = map;
		this.keys = map.keys();
	}

	public inline function hasNext():Bool {
		return keys.hasNext();
	}

	public inline function next():KeyValuePair<K,V> {
		var key = keys.next();
		return new KeyValuePair<K,V>(key, map.get(key));
	}
}

private class KeyValuePair<K,V> {
	public var key (default,null):K;
	public var value (default,null):Null<V>;

	public inline function new(key:K, value:Null<V>) {
		this.key = key;
		this.value = value;
	}
}