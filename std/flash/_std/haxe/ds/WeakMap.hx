package haxe.ds;

import flash.utils.Dictionary;

@:coreApi
class WeakMap<K:{},V> implements haxe.Constraints.IMap<K,V> {
	var d:Dictionary;
	
	public function new() : Void {
		d = new Dictionary(true);
	}

	public inline function set( key : K, value : V ) : Void {
		untyped d[key] = value;
	}

	public inline function get( key : K ) : Null<V> {
		return untyped d[key];
	}

	public inline function exists( key : K ) : Bool {
		return untyped __in__(key,d);
	}

	public function remove( key : K ) : Bool {
		if( !exists(key) ) return false;
		untyped __delete__(d,key);
		return true;
	}

	#if as3

 	public function keys() : WeakMapKeysIterator<K, V> {
		var array:Array<K> = untyped __keys__(d);
		return new WeakMapKeysIterator<K, V>(untyped array, 0, array.length);
 	}

 	public function iterator() : WeakMapValuesIterator<K, V> {
		var ret:Array<V> = [];
		for (i in keys())
			ret.push(get(i));
		return new WeakMapValuesIterator<K, V>(ret, 0, ret.length);
 	}
	
	#else

	public inline function keys() : WeakMapKeysIterator<K, V> {
		return new WeakMapKeysIterator<K, V>(d, 0);
	}

	public inline function iterator() : WeakMapValuesIterator<K, V> {
		return new WeakMapValuesIterator<K, V>(d, 0);
	}

	#end
	
	public function toString() : String {
		var s = new StringBuf();
		s.add("{");
		var it = keys();
		for( i in it ) {
			s.add(i);
			s.add(" => ");
			s.add(Std.string(get(i)));
			if( it.hasNext() )
				s.add(", ");
		}
		s.add("}");
		return s.toString();
	}

}

#if as3
class WeakMapKeysIterator<K, V> {
	var collection:Array<K>;
	var index:Int;
	var len:Int;
	public inline function new(c:Array<K>, i:Int, l:Int) {
		this.collection = c;
		index = i;
		len = l;
	}
	public inline function hasNext():Bool {
		return index < len;
	}
	public inline function next():K {
		return collection[index++];
	}
}
class WeakMapValuesIterator<K, V> {
	var collection:Array<V>;
	var index:Int;
	var len:Int;
	public inline function new(c:Array<V>, i:Int, l:Int) {
		this.collection = c;
		index = i;
		len = l;
	}
	public inline function hasNext():Bool {
		return index < len;
	}
	public inline function next():V {
		return collection[index++];
	}
}
#else
class WeakMapKeysIterator<K, V> {
	var collection:Dictionary;
	var index:Int;
	public inline function new(c:Dictionary, i:Int) {
		this.collection = c;
		index = i;
	}
	public inline function hasNext():Bool {
		var c = collection;
		var i = index;
		var result = untyped __has_next__(c, i);
		collection = c;
		index = i;
		return result;
	}
	public inline function next():K {
		return untyped __forin__(collection, index);
	}
}

class WeakMapValuesIterator<K, V> {
	var collection:Dictionary;
	var index:Int;
	public inline function new(c:Dictionary, i:Int) {
		this.collection = c;
		index = i;
	}
	public inline function hasNext():Bool {
		var c = collection;
		var i = index;
		var result = untyped __has_next__(c, i);
		collection = c;
		index = i;
		return result;
	}
	public inline function next():V {
		return untyped __foreach__(collection, index);
	}
}
#end
