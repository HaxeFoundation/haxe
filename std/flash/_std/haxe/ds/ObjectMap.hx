package haxe.ds;

class ObjectMap<K,V> extends flash.utils.Dictionary {

	public inline function get( key : K ) : Null<V> {
		return untyped this[key];
	}

	public inline function set( key : K, value : V ):Void {
		untyped this[key] = value;
	}

	public inline function exists( key : K ) : Bool {
		return untyped this[key] != null;
	}

	public inline function remove( key : K ):Bool {
		return untyped __delete__(this,key);
	}

	public function keys() : Array<K> {
		return untyped __keys__(this);
	}

	public function iterator() : Iterator<V> {
		return untyped __keys__(this).iterator();
	}

	public function toString() : String {
		var s = "";
		var it = keys();
		for( i in it ) {
			s += (s == "" ? "" : ",") + i;
			s += " => ";
			s += Std.string(get(i));
		}
		return s + "}";
	}
}