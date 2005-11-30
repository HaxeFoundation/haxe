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

	public function iterator() : Iterator<T> {
		return new ListIter<T>(h);
	}

}
