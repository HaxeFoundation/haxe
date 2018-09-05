package js.lib;

typedef Iterator<T> = {
	function next():IteratorStep<T>;
}

typedef IteratorStep<T> = {
	done:Bool,
	?value:T
}
