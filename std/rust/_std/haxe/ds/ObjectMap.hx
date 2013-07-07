package haxe.ds;
import rust.ds.LinearMap;
import rust.BaseIter;
class ObjectMap<K:{}, V> implements Map.IMap<K, V> {
	var _:LinearMap<K, V>;
	public function new(weakKeys:Bool = false):Void {
		_ = new LinearMap<K, V>();
	}
	public inline function set(key:K, value:V):Void {
		_.insert(key, value);
	}
	public inline function get(key:K):Null<V> {
		return _.find(key);
	}
	public inline function exists(key:K):Bool {
		return _.contains_key(key);
	}
	public inline function remove(key:K):Bool {
		return _.remove(key);
	}
	public inline function keys():Iterator<K> {
		return Array.of(_).iterator();
	}
	public inline function iterator():Iterator<V> {
		return null;
	}
	public inline function toString():String {
		return "Map";
	}
}