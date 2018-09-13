package js;

// prefixed with Js to avoid name clashes with standard Iterator structure

typedef JsIterator<T> = {
	function next():JsIteratorStep<T>;
}

typedef JsIteratorStep<T> = {
	done:Bool,
	?value:T
}

class JsIteratorAdapter<T> {
	var i:JsIterator<T>;
	var s:JsIteratorStep<T>;

	public inline function new(i) {
		this.i = i;
	}

	public inline function hasNext() {
		if (s == null) s = i.next();
		return !s.done;
	}

	public inline function next() {
		var v = s.value;
		s = i.next();
		return v;
	}
}
