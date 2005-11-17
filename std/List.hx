enum Cell<T> {
	empty;
	cons( item : T, next : Cell<T> );
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

	public function iterator() : Void -> T {
		var h = this.h;
		return function() {
			return switch h {
			case empty:
				done;
			case cons(it,next):
				h = next;
				it;
			}
		};
	}

}
