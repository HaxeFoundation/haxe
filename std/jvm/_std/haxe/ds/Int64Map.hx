package haxe.ds;

@:coreApi
class Int64Map<T> implements haxe.Constraints.IMap<haxe.Int64, T> {
	var hashMap:java.util.HashMap<haxe.Int64, T>;

	@:overload
	public function new():Void {
		hashMap = new java.util.HashMap();
	}

	@:overload
	function new(hashMap:java.util.HashMap<haxe.Int64, T>):Void {
		this.hashMap = hashMap;
	}

	public function set(key:haxe.Int64, value:T):Void {
		hashMap.put(key, value);
	}

	public function get(key:haxe.Int64):Null<T> {
		return hashMap.get(key);
	}

	public function exists(key:haxe.Int64):Bool {
		return hashMap.containsKey(key);
	}

	public function remove(key:haxe.Int64):Bool {
		var has = exists(key);
		hashMap.remove(key);
		return has;
	}

	public inline function keys():Iterator<haxe.Int64> {
		return hashMap.keySet().iterator();
	}

	@:runtime public inline function keyValueIterator():KeyValueIterator<haxe.Int64, T> {
		return new haxe.iterators.MapKeyValueIterator(this);
	}

	public inline function iterator():Iterator<T> {
		return hashMap.values().iterator();
	}

	public function copy():Int64Map<T> {
		return new Int64Map(hashMap.clone());
	}

	public function toString():String {
		var s = new StringBuf();
		s.add("[");
		var it = keys();
		for (i in it) {
			s.add(i.toString());
			s.add(" => ");
			s.add(Std.string(get(i)));
			if (it.hasNext())
				s.add(", ");
		}
		s.add("]");
		return s.toString();
	}

	public function clear():Void {
		hashMap.clear();
	}

	public function size():Int {
		return hashMap.size();
	}
}
