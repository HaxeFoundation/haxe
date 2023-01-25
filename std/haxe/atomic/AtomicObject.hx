package haxe.atomic;

#if !(target.atomics || core_api)
#error "This target does not support atomic operations."
#end

#if (js || hxcpp)
#error "JavaScript and Hxcpp do not support AtomicObject"
#end

/**
	Atomic object. Use with care, this does not magically make it thread-safe to mutate objects.
	Not supported on JavaScript or C++.
**/
@:coreType
abstract AtomicObject<T:{}> {
	public function new(value:T):Void;

	/**
		Atomically compares the value of `a` with `expected` and replaces `a` with `replacement` if they are equal..
		Returns the original value of `a`.

		Note that comparison is done by reference, and not by value.
		While this is expected for most objects, this might give unexpected behaviour for strings.
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
