package haxe.ds;

@:coreApi
abstract ObjectMap(flash.utils.Dictionary)<K, V> {
	public inline function new(weakKeys : Bool = false) {
		this = new flash.utils.Dictionary(weakKeys);
	}
	
	public inline function get( k : K ) : Null<V> {
		return untyped this[k];
	}

	public inline function set( k : K, v : V ):Void {
		untyped this[k] = v;
	}

	public inline function exists( k : K ) : Bool {
		return untyped this[k] != null;
	}

	public inline function remove( k : K ):Bool {
		return untyped __delete__(this,k);
	}

	public inline function keys() : Array<K> {
		return untyped __keys__(this);
	}

	public function iterator() : Iterator<V> {
		return untyped __keys__(this).iterator();
	}
}