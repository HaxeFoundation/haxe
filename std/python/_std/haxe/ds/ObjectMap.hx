package haxe.ds;

import python.lib.Builtin;
import python.lib.Dict;

class ObjectMap<K:{},V> implements haxe.Constraints.IMap<K, V> {

	var h : Dict<K,V>;


	public function new() : Void {
		h = new Dict();
	}

	public function set(key:K, value:V):Void {
		h.set(key, value);
	}

	public inline function get(key:K):Null<V> {
		return h.get(key, null);
	}

	public inline function exists(key:K):Bool {
		return h.hasKey(key);
	}

	public function remove( key : K ) : Bool
	{
		var r = h.hasKey(key);
		if (r) h.remove(key);
		return r;
	}

	public function keys() : Iterator<K> {
		return h.keys().iter();
	}

	public function iterator() : Iterator<V> {
		return h.values().iter();
	}

	public function toString() : String {
		var s = new StringBuf();
		s.add("{");
		var it = keys();
		for( i in it ) {
			s.add(Std.string(i));
			s.add(" => ");
			s.add(Std.string(get(i)));
			if( it.hasNext() )
				s.add(", ");
		}
		s.add("}");
		return s.toString();
	}
}