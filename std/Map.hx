@:multiType
abstract Map(IMap < K, V > )<K,V> {
	public function new();

	@:to static inline function toHash(t:IMap < String, V > ):haxe.ds.StringMap<V> {
		return new haxe.ds.StringMap<V>();
	}

	@:to static inline function toIntHash(t:IMap < Int, V > ):haxe.ds.IntMap<V> {
		return new haxe.ds.IntMap<V>();
	}

	@:to static inline function toHashMap<K:{ function hashCode():Int; }>(t:IMap < K, V >):haxe.ds.HashMap<K,V> {
		return new haxe.ds.HashMap<K, V>();
	}

	public inline function set(k:K, v:V) this.set(k, v)
	public inline function get(k:K) return this.get(k)
	public inline function exists(k:K) return this.exists(k)
	public inline function remove(k:K) return this.remove(k)
	public inline function keys() return this.keys()
	public inline function iterator() return this.iterator()
}

private typedef IMap < K, V > = {
	public function get(k:K):V;
	public function set(k:K, v:V):Void;
	public function exists(k:K):Null<V>;
	public function remove(k:K):Bool;
	public function keys():Iterator<K>;
	public function iterator():Iterator<V>;
}
