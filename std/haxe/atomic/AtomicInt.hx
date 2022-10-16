package haxe.atomic;

#if !(target.atomics || core_api)
#error "This target does not support atomic operations."
#end

/**
	Atomic integer.
	(js) The Atomics and SharedArrayBuffer objects need to be available. Errors will be thrown if this is not the case.
**/
@:coreType
abstract AtomicInt {
	public function new(value:Int):Void;

	/**
		Atomically adds `b` to `a`.
		Returns the original value of `a`.
	**/
	public function add(b:Int):Int;

	/**
		Atomically substracts `b` from `a`.
		Returns the original value of `a`.
	**/
	public function sub(b:Int):Int;

	/**
		Atomically computes the bitwise and of `a` and `b` and stores it in `a`.
		Returns the original value of `a`.
	**/
	public function and(b:Int):Int;

	/**
		Atomically computes the bitwise or of `a` and `b` and stores it in `a`.
		Returns the original value of `a`.
	**/
	public function or(b:Int):Int;

	/**
		Atomically computes the bitwise xor of `a` and `b` and stores it in `a`.
		Returns the original value of `a`.
	**/
	public function xor(b:Int):Int;

	/**
		Atomically compares the value of `a` with `expected` and replaces `a` with `replacement` if they are equal..
		Returns the original value of `a`.
	**/
	public function compareExchange(expected:Int, replacement:Int):Int;

	/**
		Atomically exchanges `a` with `value`.
		Returns the original value of `a`.
	**/
	public function exchange(value:Int):Int;

	/**
		Atomically fetches the value of `a`.
	**/
	public function load():Int;

	/**
		Atomically stores `value` into `a`.
		Returns the value that has been stored.
	**/
	public function store(value:Int):Int;
}
