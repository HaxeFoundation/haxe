package haxe.atomic;

#if cppia
#error "Cppia does not support atomic operations yet."
#end

@:native("volatile int")
@:scalar
@:coreType
private extern abstract VolatileInt from Int to Int {}

extern abstract AtomicInt(cpp.Pointer<VolatileInt>) {
	public extern inline function new(value:Int) {
		this = cpp.Pointer.ofArray([(cast value:VolatileInt)]);
	}

	@:native("_hx_atomic_add")
	public extern function add(b:Int):Int;

	@:native("_hx_atomic_sub")
	public extern function sub(b:Int):Int;

	@:native("_hx_atomic_and")
	public extern function and(b:Int):Int;

	@:native("_hx_atomic_or")
	public extern function or(b:Int):Int;

	@:native("_hx_atomic_xor")
	public extern function xor(b:Int):Int;

	@:native("_hx_atomic_compare_exchange")
	public extern function compareExchange(expected:Int, replacement:Int):Int;

	@:native("_hx_atomic_exchange")
	public extern function exchange(value:Int):Int;

	@:native("_hx_atomic_load")
	public extern function load():Int;

	@:native("_hx_atomic_store")
	public extern function store(value:Int):Int;
}
