package haxe.coro.context;

/**
	Represents an element of `Context`. See `haxe.coro.schedulers.Scheduler` for a usage example.
**/
interface IElement<T> {
	/**
		Returns the `Key` of `this` context element.
	**/
	public function getKey():Key<T>;
}
