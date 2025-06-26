package haxe.atomic;

#if !(target.atomics || core_api)
#error "Atomic operations are not supported on this target!"
#end

/**
	Atomic boolean.
	(js) The Atomics and SharedArrayBuffer objects need to be available. Errors will be thrown if this is not the case.
**/
@:coreApi
@:coreType
abstract AtomicBool {
	public function new(value:Bool):Void;

	/**
		Atomically compares the value of `a` with `expected` and replaces `a` with `replacement` if they are equal..
		Returns the original value of `a`.
	**/
	public function compareExchange(expected:Bool, replacement:Bool):Bool;

	/**
		Atomically exchanges `a` with `value`.
		Returns the original value of `a`.
	**/
	public function exchange(value:Bool):Bool;
	/**
		Atomically fetches the value of `a`.
	**/
	public function load():Bool;
	/**
		Atomically stores `value` into `a`.
		Returns the value that has been stored.
	**/
	public function store(value:Bool):Bool;
}