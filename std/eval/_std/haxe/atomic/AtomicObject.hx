package haxe.atomic;

private typedef AtomicObjectData<T:{}> = Any;

/**
	Atomic object. Use with care, this does not magically make it thread-safe to mutate objects.
	Not supported on JavaScript.
**/
extern abstract AtomicObject<T:{}>(AtomicObjectData<T>) {
	public function new(value:T):Void;

	/**
		Atomically compares the value of `a` with `expected` and replaces `a` with `replacement` if they are equal.
		Returns the original value of `a`.
	**/
	public function compareExchange(expected:T, replacement:T):T;

	/**
		Atomically exchanges `a` with `value`.
		Returns the original value of `a`.
	**/
	public function exchange(value:T):T;

	/**
		Atomically fetches the value of `a`.
	**/
	public function load():T;

	/**
		Atomically stores `value` into `a`.
		Returns the value that has been stored.
	**/
	public function store(value:T):T;
}
