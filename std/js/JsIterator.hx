package js;

/**
	JsIterator is prefixed with Js to avoid name clashes with standard 
	Iterator structure.
**/
typedef JsIterator<T> = {
	function next():JsIteratorStep<T>;
}

/**
	Object returned by `JsIterator.next`.
**/
typedef JsIteratorStep<T> = {
	done:Bool,
	?value:T
}
