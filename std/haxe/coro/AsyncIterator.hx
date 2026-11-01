package haxe.coro;

/**
	An `AsyncIterator` is like `Iterator`, but `hasNext()` is a coroutine. This
	allows iterating over asynchronous data sources such as channels or network
	streams using `for` loops in coroutine contexts.

	@see `AsyncIterable`
**/
typedef AsyncIterator<T> = {
	/**
		Returns `false` if the iteration is complete, `true` otherwise.

		This function is a coroutine and may suspend while waiting for the next
		element to become available.
	**/
	@:coroutine function hasNext():Bool;

	/**
		Returns the current item of the `AsyncIterator` and advances to the next one.
	**/
	function next():T;
}
