package haxe;

#if (!target.atomics)
#error "This target does not support atomic operations."
#end

// only Atomic<Int> can be implemented in a portable way at the moment
// the type parameter is there so that more types can be implemented without breaking anything (hopefully)

/**
	Atomic operations.
	(js) The Atomics and SharedArrayBuffer objects need to be available. Errors will be thrown if this is not the case.
**/
@:coreType
@:coreApi
@:using(haxe.Atomic)
abstract Atomic<T:Int> {
	public extern function new(value:T):Void;

	/**
		Atomically adds `b` to `a`.
		Returns the original value of `a`.
	**/
	public extern static function add<T:Int>(a:Atomic<T>, b:T):T;

	/**
		Atomically substracts `b` from `a`.
		Returns the original value of `a`.
	**/
	public extern static function sub<T:Int>(a:Atomic<T>, b:T):T;

	/**
		Atomically computes the bitwise and of `a` and `b` and stores it in `a`.
		Returns the original value of `a`.
	**/
	public extern static function and<T:Int>(a:Atomic<T>, b:T):T;

	/**
		Atomically computes the bitwise or of `a` and `b` and stores it in `a`.
		Returns the original value of `a`.
	**/
	public extern static function or<T:Int>(a:Atomic<T>, b:T):T;

	/**
		Atomically computes the bitwise xor of `a` and `b` and stores it in `a`.
		Returns the original value of `a`.
	**/
	public extern static function xor<T:Int>(a:Atomic<T>, b:T):T;

	/**
		Atomically compares the value of `a` with `expected` and replaces `a` with `replacement` if they are equal..
		Returns the original value of `a`.
	**/
	public extern static function compareExchange<T:Int>(a:Atomic<T>, expected:T, replacement:T):T;

	/**
		Atomically exchanges `a` with `value`.
		Returns the original value of `a`.
	**/
	public extern static function exchange<T:Int>(a:Atomic<T>, value:T):T;

	/**
		Atomically fetches the value of `a`.
	**/
	public extern static function load<T:Int>(a:Atomic<T>):T;

	/**
		Atomically stores `value` into `a`.
		Returns the value that has been stored.
	**/
	public extern static function store<T:Int>(a:Atomic<T>, value:T):T;
}
