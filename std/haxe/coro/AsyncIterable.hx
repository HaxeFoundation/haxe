package haxe.coro;

/**
	An `AsyncIterable` is a data structure which has an `iterator()` method
	returning an `AsyncIterator`. This allows asynchronous data sources to be
	used in `for` loops within coroutine contexts.

	@see `AsyncIterator`
**/
typedef AsyncIterable<T> = {
	function iterator():AsyncIterator<T>;
}
