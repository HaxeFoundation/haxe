package js;

// prefixed with Js to avoid name clashes with standard Iterator structure

typedef JsIterator<T> = {
	function next():JsIteratorStep<T>;
}

typedef JsIteratorStep<T> = {
	done:Bool,
	?value:T
}
