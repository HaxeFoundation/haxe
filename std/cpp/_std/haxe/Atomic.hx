package haxe;

import cpp.AtomicInt;

@:native("volatile int")
@:scalar
private abstract VolatileInt(Int) {
	@:to function toInt():Int {
		return cast this;
	}
}

@:coreApi
@:using(haxe.Atomic)
extern abstract Atomic<T:Int>(cpp.Pointer<VolatileInt>) {
	public inline function new(value:T) {
		this = cpp.Pointer.ofArray([]);
	}

	@:native("_hx_atomic_add")
	public extern static function add<T:Int>(a:Atomic<T>, b:T):T;

	@:native("_hx_atomic_sub")
	public extern static function sub<T:Int>(a:Atomic<T>, b:T):T;

	@:native("_hx_atomic_and")
	public extern static function and<T:Int>(a:Atomic<T>, b:T):T;

	@:native("_hx_atomic_or")
	public extern static function or<T:Int>(a:Atomic<T>, b:T):T;

	@:native("_hx_atomic_xor")
	public extern static function xor<T:Int>(a:Atomic<T>, b:T):T;

	@:native("_hx_atomic_compare_exchange")
	public extern static function compareExchange<T:Int>(a:Atomic<T>, expected:T, replacement:T):T;

	@:native("_hx_atomic_exchange")
	public extern static function exchange<T:Int>(a:Atomic<T>, value:T):T;

	@:native("_hx_atomic_load")
	public extern static function load<T:Int>(a:Atomic<T>):T;

	@:native("_hx_atomic_store")
	public extern static function store<T:Int>(a:Atomic<T>, value:T):T;
}
