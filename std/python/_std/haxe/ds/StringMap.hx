package haxe.ds;


import python.Syntax;
import python.lib.Dict;

class StringMap<T> implements haxe.Constraints.IMap<String, T> {
	private var h : Dict<String,T>;

	public function new() : Void {
		h = new Dict();
	}

	public inline function set( key : String, value : T ) : Void {
		h.set(key, value);
	}

	public inline function get( key : String ) : Null<T> {
		return h.get(key, null);
	}

	public inline function exists( key : String ) : Bool {
		return h.hasKey(key);
	}

	public function remove( key : String ) : Bool {
		var has = h.hasKey(key);
		if (has) h.remove(key);
		return has;
	}

	public function keys() : Iterator<String> {
		return h.keys().iter();
	}

	public function iterator() : Iterator<T> {
		return h.values().iter();
	}

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