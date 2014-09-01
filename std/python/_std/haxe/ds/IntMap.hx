package haxe.ds;

import python.lib.Dict;
import python.Syntax;

class IntMap<T> implements haxe.Constraints.IMap<Int, T> {
	private var h : Dict<Int, T>;

	public function new() : Void {
		h = new Dict();
	}

	public function set( key : Int, value : T ) : Void {
		h.set(key, value);
	}

	public inline function get( key : Int ) : Null<T> {
		return h.get(key, null);
	}

	public inline function exists( key : Int ) : Bool {
		return h.hasKey(key);
	}

	public function remove( key : Int ) : Bool
	{
		if(!h.hasKey(key)) return false;
		Syntax.delete(Syntax.arrayAccess(h, key));
		return true;
	}

	public function keys() : Iterator<Int> {
		return h.keys().iter();
	}

	public function iterator() : Iterator<T> {
		var iter = keys();
		var ref = h;
		return {
			hasNext : function() { return iter.hasNext(); },
			next : function() { var i = iter.next(); return ref.get(i, null); }
		};
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