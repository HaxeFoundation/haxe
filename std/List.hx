enum Cell<T> {
	empty;
	cons( item : T, next : Cell<T> );
}

class ListIter<T> implements Iterator<T> {

	private var h : Cell<T>;

	public function new( h : Cell<T> ) {
		this.h = h;
	}

	public function hasNext() : Bool {
		return (h != empty);
	}

	public function next() : T {
		return switch h {
		case empty:
			null;
		case cons(it,h):
			this.h = h;
			it;
		}
	}

}


class List<T> {

	private var h : Cell<T>;

	public function new() {
		h = empty;
	}

	public function push( item : T ) : Void {
		h = cons(item,h);
	}

	public function pop() : T {
		return switch h {
		case empty:
			null;
		case cons(it,h):
			this.h = h;
			it;
		}
	}

	public function remove( v : T ) : Bool {
		var loop;
		var found = { ref : false };
		loop = function(h) {
			return switch h {
			case empty:
				empty;
			case cons(v2,h):
				if( v2 == v ) {
					found.ref = true;
					h;
				} else
					cons(v2,loop(h));
			}
		};
		h = loop(h);
		return found.ref;
	}

	public function iterator() : Iterator<T> {
		return new ListIter<T>(h);
	}

	public function toString() {
		var s = new StringBuf();
		var it = iterator();
		s.add("{");
		for i in it {
			s.add(i);
			if( it.hasNext() )
				s.add(", ");
		}
		s.add("}");
		return s.toString();
	}

}
