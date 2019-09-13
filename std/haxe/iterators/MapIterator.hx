package haxe.iterators;

import haxe.ds.EnumValueMap;
import haxe.ds.HashMap;
import haxe.ds.IntMap;
import haxe.ds.ObjectMap;
import haxe.ds.StringMap;
import haxe.ds.WeakMap;
import haxe.Constraints.IMap;

class MapGenericIterator {
	static public inline function iterator<K, V>(m:Map<K, V>):Iterator<V> {
		return (cast m:IMap<K, V>).iterator();
	}

	static public inline function keys<K, V>(m:Map<K, V>):Iterator<K> {
		return (cast m:IMap<K, V>).keys();
	}
}

class MapEnumValueIterator {
	static public inline function iterator<K:EnumValue, V>(m:EnumValueMap<K,V>) {
		return m.iterator();
	}

	static public inline function keys<K:EnumValue, V>(m:EnumValueMap<K,V>) {
		return m.keys();
	}
}

class MapHashIterator {
	static public inline function iterator<K:{function hashCode():Int;}, V>(m:HashMap<K,V>) {
		return m.iterator();
	}

	static public inline function keys<K:{function hashCode():Int;}, V>(m:HashMap<K,V>) {
		return m.keys();
	}
}

class MapIntIterator {
	static public inline function iterator<V>(m:IntMap<V>) {
		return m.iterator();
	}

	static public inline function keys<K:Int, V>(m:Map<K, V>):Iterator<K> {
		return cast ((m:IntMap<V>).keys());
	}
}

class MapObjectIterator {
	static public inline function iterator<K:{}, V>(m:ObjectMap<K,V>) {
		return m.iterator();
	}

	static public inline function keys<K:{}, V>(m:ObjectMap<K,V>) {
		return m.keys();
	}
}

class MapStringIterator {
	static public inline function iterator<V>(m:StringMap<V>) {
		return m.iterator();
	}

	static public inline function keys<K:String, V>(m:Map<K, V>):Iterator<K> {
		return cast ((m:StringMap<V>).keys());
	}
}

class MapWeakIterator {
	static public inline function iterator<K:{}, V>(m:WeakMap<K, V>) {
		return m.iterator();
	}

	static public inline function keys<K:{}, V>(m:WeakMap<K, V>) {
		return m.keys();
	}
}